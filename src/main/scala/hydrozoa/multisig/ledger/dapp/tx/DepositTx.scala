package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantizeLosslessUnsafe, toEpochQuantizedInstant}
import hydrozoa.lib.cardano.scalus.ledger.api.TransactionOutputEncoders.given
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.explainConst
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import io.bullet.borer.{Cbor, Encoder}
import monocle.{Focus, Lens}
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, platform}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}

final case class DepositTx private (
    depositProduced: DepositUtxo,
    validityEnd: QuantizedInstant,
    override val tx: Transaction,
    override val txLens: Lens[DepositTx, Transaction] = Focus[DepositTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[DepositTx]

object DepositTx {
    case class Config(cardanoInfo: CardanoInfo, headAddress: ShelleyAddress, txTiming: TxTiming) {
        def evaluator: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluateAndComputeCost)

    }

    final case class Builder(
        config: Config,
        partialRefundTx: RefundTx.Builder.PartialResult[RefundTx.PostDated],
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[GenesisObligation],
        donationToTreasury: Coin,
        changeAddress: ShelleyAddress,
    ) {
        def build(): Either[(SomeBuildError, String), DepositTx] = {
            import partialRefundTx.refundInstructions

            val virtualOutputsCbor: Array[Byte] =
                Cbor.encode(virtualOutputs.toList.map(_.toBabbage)).toByteArray

            val virtualOutputsHash: Hash32 = Hash[Blake2b_256, Any](
              platform.blake2b_256(ByteString.unsafeFromArray(virtualOutputsCbor))
            )

            val stepRefundMetadata =
                ModifyAuxiliaryData(_ =>
                    Some(
                      MD(
                        MD.Deposit(
                          headAddress = config.headAddress,
                          depositUtxoIx = 0, // This builder produces the deposit utxo at index 0
                          virtualOutputsHash = virtualOutputsHash
                        )
                      )
                    )
                )

            val spendUtxosFunding = utxosFunding.toList.map(Spend(_, PubKeyWitness))

            val depositValue = partialRefundTx.inputValueNeeded

            val depositDatum: DepositUtxo.Datum = DepositUtxo.Datum(refundInstructions)

            val rawDepositProduced = TransactionOutput.Babbage(
              address = config.headAddress,
              value = depositValue,
              datumOption = Some(DatumOption.Inline(toData(depositDatum))),
              scriptRef = None
            )

            val sendDeposit = Send(rawDepositProduced)

            val sendChange = Send(
              TransactionOutput.Babbage(
                address = changeAddress,
                value = Value.zero,
                datumOption = None,
                scriptRef = None
              )
            )

            val validityEndQuantizedInstant =
                partialRefundTx.refundInstructions.startTime.toEpochQuantizedInstant(
                  config.cardanoInfo.slotConfig
                )
                    - config.txTiming.depositAbsorptionDuration
                    - config.txTiming.depositMaturityDuration
                    - config.txTiming.silenceDuration
            val ttl = ValidityEndSlot(validityEndQuantizedInstant.toSlot.slot)

            for {
                ctx <- TransactionBuilder
                    .build(
                      config.cardanoInfo.network,
                      spendUtxosFunding ++ List(stepRefundMetadata, sendDeposit, sendChange, ttl)
                    )
                    .explainConst("building unbalanced deposit tx failed")

                finalized <- ctx
                    .finalizeContext(
                      config.cardanoInfo.protocolParams,
                      diffHandler = Change
                          .changeOutputDiffHandler(_, _, config.cardanoInfo.protocolParams, 1),
                      evaluator = config.evaluator,
                      validators = Tx.Validators.nonSigningNonValidityChecksValidators
                    )
                    .explainConst("balancing deposit tx failed")

                tx = finalized.transaction

                depositProduced = DepositUtxo(
                  TransactionInput(tx.id, 0),
                  config.headAddress,
                  depositDatum,
                  rawDepositProduced.value,
                  virtualOutputs
                )
            } yield DepositTx(
              depositProduced,
              validityEndQuantizedInstant,
              tx
            )
        }
    }

    sealed trait ParseError extends Throwable

    case class MetadataParseError(e: MD.ParseError) extends ParseError

    case class HashMismatchVirtualOutputs(
        virtualOutputs: NonEmptyList[GenesisObligation],
        hash: Hash32
    ) extends ParseError

    case class MissingDepositOutputAtIndex(e: Int) extends ParseError

    case class DepositUtxoError(e: DepositUtxo.DepositUtxoConversionError) extends ParseError

    case class TxCborDeserializationFailed(e: Throwable) extends ParseError

    case class ValidityEndParseError(e: Throwable) extends ParseError

    /** Parse a deposit transaction, ensuring that there is exactly one Babbage Utxo at the head
      * address (given in the transaction metadata) with an Inline datum that parses correctly.
      */
    def parse(
        txBytes: Tx.Serialized,
        config: DepositTx.Config,
        virtualOutputs: NonEmptyList[GenesisObligation]
    ): Either[ParseError, DepositTx] = {
        given ProtocolVersion = config.cardanoInfo.protocolParams.protocolVersion
        given OriginalCborByteArray = OriginalCborByteArray(txBytes)

        val virtualOutputsList = virtualOutputs.toList

        io.bullet.borer.Cbor.decode(txBytes).to[Transaction].valueTry match {
            case Success(tx) =>
                for {
                    // Pull head address from metadata
                    d <- MD
                        .parse(tx) match {
                        case Right(d: Metadata.Deposit) => Right(d)
                        case Right(o) => Left(MetadataParseError(MD.UnexpectedTxType(o, "Deposit")))
                        case Left(e)  => Left(MetadataParseError(e))
                    }
                    Metadata.Deposit(headAddress, depositUtxoIx, virtualOutputsHash) = d

                    // Compare hash with virtual outputs
                    virtualOutputsCbor: Array[Byte] = Cbor
                        .encode(virtualOutputsList.map(_.toBabbage))
                        .toByteArray
                    calculatedVirtualOutputsHash: Hash32 = Hash[Blake2b_256, Any](
                      platform.blake2b_256(ByteString.unsafeFromArray(virtualOutputsCbor))
                    )
                    _ <- Either.cond(
                      virtualOutputsHash == calculatedVirtualOutputsHash,
                      (),
                      HashMismatchVirtualOutputs(virtualOutputs, virtualOutputsHash)
                    )

                    // Grab the deposit output at the index specified in the metadata
                    depositOutput <- tx.body.value.outputs
                        .lift(depositUtxoIx)
                        .toRight(MissingDepositOutputAtIndex(depositUtxoIx))

                    validityEnd <- Try {
                        val ttlSlot = tx.body.value.ttl.get
                        val ttlPosixMillis = config.cardanoInfo.slotConfig.slotToTime(ttlSlot)
                        val instant = java.time.Instant.ofEpochMilli(ttlPosixMillis)
                        instant.quantizeLosslessUnsafe(config.cardanoInfo.slotConfig)
                    } match {
                        case Failure(exception) => Left(ValidityEndParseError(exception))
                        case Success(v)         => Right(v)
                    }

                    depositUtxo <- DepositUtxo
                        .fromUtxo(
                          Utxo(TransactionInput(tx.id, depositUtxoIx), depositOutput.value),
                          config.headAddress,
                          virtualOutputs,
                          absorptionStart = validityEnd + config.txTiming.depositMaturityDuration,
                          absorptionEnd =
                              validityEnd + config.txTiming.depositMaturityDuration + config.txTiming.depositAbsorptionDuration
                        )
                        .left
                        .map(DepositUtxoError(_))

                } yield DepositTx(depositUtxo, validityEnd, tx)
            case Failure(e) => Left(TxCborDeserializationFailed(e))
        }

    }
}

package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantizeLosslessUnsafe, toEpochQuantizedInstant}
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.TransactionOutputEncoders.given
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.explainConst
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.tx.GenesisObligation
import io.bullet.borer.{Cbor, Encoder}
import monocle.{Focus, Lens}
import scala.util.{Failure, Success, Try}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, platform}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}

final case class DepositTx(
    depositProduced: DepositUtxo,
    validityEnd: QuantizedInstant,
    override val tx: Transaction,
    override val txLens: Lens[DepositTx, Transaction] = Focus[DepositTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[DepositTx]

object DepositTx {
    export DepositTxOps.{Build, Parse}
}

private object DepositTxOps {
    type Config = CardanoNetwork.Section & HeadPeers.Section & TxTiming.Section
    // FIXME: We need InitialBlock.Section in the config, so that we can add
    //  multisigRegimeUtxo as a reference input (not for its script).
    //  This is how the deposit tx gets rolled back if the init tx is rolled back.

    final case class Build(config: Config)(
        partialRefundTx: RefundTx.PartialResult[RefundTx.PostDated],
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[GenesisObligation],
        donationToTreasury: Coin,
        changeAddress: ShelleyAddress,
    ) {
        def result: Either[(SomeBuildError, String), DepositTx] = {
            import partialRefundTx.refundInstructions

            val virtualOutputsRaw: List[TransactionOutput.Babbage] =
                virtualOutputs.toList.map(_.toBabbage)

            val virtualOutputsCbor: Array[Byte] = Cbor.encode(virtualOutputsRaw).toByteArray

            val virtualOutputsHash: Hash32 = Hash[Blake2b_256, Any](
              platform.blake2b_256(ByteString.unsafeFromArray(virtualOutputsCbor))
            )

            val stepRefundMetadata =
                ModifyAuxiliaryData(_ =>
                    Some(
                      MD(
                        MD.Deposit(
                          headAddress = config.headMultisigAddress,
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
              address = config.headMultisigAddress,
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
                  config.slotConfig
                )
                    - config.txTiming.depositAbsorptionDuration
                    - config.txTiming.depositMaturityDuration
                    - config.txTiming.silenceDuration
            val ttl = ValidityEndSlot(validityEndQuantizedInstant.toSlot.slot)

            for {
                ctx <- TransactionBuilder
                    .build(
                      config.network,
                      spendUtxosFunding ++ List(stepRefundMetadata, sendDeposit, sendChange, ttl)
                    )
                    .explainConst("building unbalanced deposit tx failed")

                finalized <- ctx
                    .finalizeContext(
                      config.cardanoProtocolParams,
                      diffHandler = Change
                          .changeOutputDiffHandler(_, _, config.cardanoProtocolParams, 1),
                      evaluator = config.plutusScriptEvaluatorForTxBuild,
                      validators = Tx.Validators.nonSigningNonValidityChecksValidators
                    )
                    .explainConst("balancing deposit tx failed")

                tx = finalized.transaction

                depositProduced = DepositUtxo(
                  TransactionInput(tx.id, 0),
                  config.headMultisigAddress,
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

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error extends Throwable {
            case MetadataParseError(e: MD.ParseError)
            case HashMismatchVirtualOutputs(
                virtualOutputs: NonEmptyList[GenesisObligation],
                hash: Hash32
            )
            case MissingDepositOutputAtIndex(e: Int)
            case DepositUtxoError(e: DepositUtxo.DepositUtxoConversionError)
            case TxCborDeserializationFailed(e: Throwable)
            case ValidityEndParseError(e: Throwable)
        }
    }

    /** Parse a deposit transaction, ensuring that there is exactly one Babbage Utxo at the head
      * address (given in the transaction metadata) with an Inline datum that parses correctly.
      */
    final case class Parse(config: Config)(
        txBytes: Tx.Serialized,
        virtualOutputs: NonEmptyList[GenesisObligation]
    ) {
        import Parse.*
        import Parse.Error.*

        given ProtocolVersion = config.cardanoProtocolVersion

        def result: ParseErrorOr[DepositTx] = {
            given OriginalCborByteArray = OriginalCborByteArray(txBytes)

            val virtualOutputsList = virtualOutputs.toList

            io.bullet.borer.Cbor.decode(txBytes).to[Transaction].valueTry match {
                case Success(tx) =>
                    for {
                        // Pull head address from metadata
                        d <- MD
                            .parse(tx) match {
                            case Right(d: Metadata.Deposit) => Right(d)
                            case Right(o) =>
                                Left(MetadataParseError(MD.UnexpectedTxType(o, "Deposit")))
                            case Left(e) => Left(MetadataParseError(e))
                        }
                        Metadata.Deposit(headAddress, depositUtxoIx, virtualOutputsHash) = d

                        // Compare hash with virtual outputs
                        virtualOutputsRaw: List[TransactionOutput.Babbage] =
                            virtualOutputsList.map(_.toBabbage)

                        virtualOutputsCbor: Array[Byte] = Cbor.encode(virtualOutputsRaw).toByteArray
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
                            val ttlPosixMillis = config.slotConfig.slotToTime(ttlSlot)
                            val instant = java.time.Instant.ofEpochMilli(ttlPosixMillis)
                            instant.quantizeLosslessUnsafe(config.slotConfig)
                        } match {
                            case Failure(exception) => Left(ValidityEndParseError(exception))
                            case Success(v)         => Right(v)
                        }

                        depositUtxo <- DepositUtxo
                            .fromUtxo(
                              Utxo(TransactionInput(tx.id, depositUtxoIx), depositOutput.value),
                              config.headMultisigAddress,
                              virtualOutputs,
                              absorptionStart =
                                  validityEnd + config.txTiming.depositMaturityDuration,
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
}

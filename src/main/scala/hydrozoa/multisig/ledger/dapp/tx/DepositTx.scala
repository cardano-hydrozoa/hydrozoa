package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantizeLosslessUnsafe}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.explainConst
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.tx.GenesisObligation
import monocle.{Focus, Lens}
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend, ValidityEndSlot}
import scalus.uplc.builtin.Data.toData

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
    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section &
        TxTiming.Section

    final case class Build(config: Config)(
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[GenesisObligation],
        depositValue: Value,
        changeAddress: ShelleyAddress,
        submissionDeadline: QuantizedInstant,
        refundInstructions: DepositUtxo.Refund.Instructions
    ) {
        def result: Either[(SomeBuildError, String), DepositTx] = {

            val referenceMultisigRegime =
                ReferenceOutput(config.multisigRegimeUtxo.asUtxo)

            val spendUtxosFunding = utxosFunding.toList.map(Spend(_, PubKeyWitness))

            val depositDatum: DepositUtxo.Datum =
                DepositUtxo.Datum(DepositUtxo.Refund.Instructions.Onchain.apply(refundInstructions))

            val sendDeposit = Send(
              TransactionOutput.Babbage(
                address = config.headMultisigAddress,
                value = depositValue,
                datumOption = Some(DatumOption.Inline(toData(depositDatum))),
                scriptRef = None
              )
            )

            val sendChange = Send(
              TransactionOutput.Babbage(
                address = changeAddress,
                value = Value.zero,
                datumOption = None,
                scriptRef = None
              )
            )

            val ttl = ValidityEndSlot(submissionDeadline.toSlot.slot)

            val addRefundMetadata =
                ModifyAuxiliaryData(_ =>
                    Some(
                      MD(
                        MD.Deposit(
                          headAddress = config.headMultisigAddress,
                          depositUtxoIx = 0, // This builder produces the deposit utxo at index 0
                          virtualOutputsHash = GenesisObligation.hash(virtualOutputs)
                        )
                      )
                    )
                )

            for {
                ctx <- TransactionBuilder
                    .build(
                      config.network,
                      spendUtxosFunding ++ List(
                        referenceMultisigRegime,
                        addRefundMetadata,
                        sendDeposit,
                        sendChange,
                        ttl
                      )
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
                  utxoId = TransactionInput(tx.id, 0),
                  address = config.headMultisigAddress,
                  datum = depositDatum,
                  value = depositValue,
                  virtualOutputs = virtualOutputs,
                  submissionDeadline = submissionDeadline
                )
            } yield DepositTx(
              depositProduced,
              submissionDeadline,
              tx
            )
        }
    }

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error extends Throwable {
            case MetadataParseError(e: MD.ParseError)
            case AlienDeposit(headAddress: ShelleyAddress)
            case HashMismatchVirtualOutputs(
                virtualOutputs: NonEmptyList[GenesisObligation],
                hash: Hash32
            )
            case MissingDepositOutputAtIndex(e: Int)
            case DepositUtxoError(e: DepositUtxo.DepositUtxoConversionError)
            case TxCborDeserializationFailed(e: Throwable)
            case ValidityEndParseError(e: Throwable)
            case MultisigRegimeWitnessUtxoNotReferenced
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

        def result: ParseErrorOr[DepositTx] = {

            given OriginalCborByteArray = OriginalCborByteArray(txBytes)
            given ProtocolVersion = config.cardanoProtocolVersion

            io.bullet.borer.Cbor.decode(txBytes).to[Transaction].valueTry match {
                case Success(tx) =>
                    for {
                        // Pull metadata
                        d <- MD
                            .parse(tx) match {
                            case Right(d: Metadata.Deposit) => Right(d)
                            case Right(o) =>
                                Left(MetadataParseError(MD.UnexpectedTxType(o, "Deposit")))
                            case Left(e) => Left(MetadataParseError(e))
                        }
                        Metadata.Deposit(headAddress, depositUtxoIx, virtualOutputsHash) = d

                        // Check head address
                        _ <- Either.cond(
                          headAddress == config.headMultisigAddress,
                          (),
                          AlienDeposit(headAddress)
                        )

                        // Compare hash with virtual outputs
                        calculatedVirtualOutputsHash = GenesisObligation.hash(virtualOutputs)
                        _ <- Either.cond(
                          virtualOutputsHash == calculatedVirtualOutputsHash,
                          (),
                          HashMismatchVirtualOutputs(virtualOutputs, virtualOutputsHash)
                        )

                        // Grab the deposit output at the index specified in the metadata
                        depositOutput <- tx.body.value.outputs
                            .lift(depositUtxoIx)
                            .toRight(MissingDepositOutputAtIndex(depositUtxoIx))

                        // Check that ttl was properly quantized
                        validityEnd <- Try {
                            val ttlSlot = tx.body.value.ttl.get
                            val ttlPosixMillis = config.slotConfig.slotToTime(ttlSlot)
                            val instant = java.time.Instant.ofEpochMilli(ttlPosixMillis)
                            instant.quantizeLosslessUnsafe(config.slotConfig)
                        } match {
                            case Failure(exception) => Left(ValidityEndParseError(exception))
                            case Success(v)         => Right(v)
                        }

                        // Check the multisig regime witness utxo was referenced
                        _ <- Either.cond(
                          tx.body.value.referenceInputs.toSet
                              .contains(config.multisigRegimeUtxo.utxoId),
                          (),
                          MultisigRegimeWitnessUtxoNotReferenced
                        )

                        depositUtxo <- DepositUtxo
                            .fromUtxo(
                              utxo =
                                  Utxo(TransactionInput(tx.id, depositUtxoIx), depositOutput.value),
                              headNativeScriptAddress = config.headMultisigAddress,
                              virtualOutputs = virtualOutputs,
                              submissionDeadline = validityEnd
                            )
                            .left
                            .map(DepositUtxoError(_))

                    } yield DepositTx(depositUtxo, validityEnd, tx)
                case Failure(e) => Left(TxCborDeserializationFailed(e))
            }
        }
    }
}

package hydrozoa.multisig.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantizeLosslessUnsafe}
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.tx.Tx.Builder.explainConst
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import monocle.{Focus, Lens}
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend, ValidityEndSlot}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{fromData, toData}

final case class DepositTx(
    depositProduced: DepositUtxo,
    submissionDeadline: QuantizedInstant,
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
        l2Payload: ByteString,
        l2Value: Value,
        depositFee: Coin,
        changeAddress: ShelleyAddress,
        requestValidityEndTime: QuantizedInstant,
        refundInstructions: DepositUtxo.Refund.Instructions
    ) {
        def result: Either[(SomeBuildError, String), DepositTx] = {

            val referenceMultisigRegime =
                ReferenceOutput(config.multisigRegimeUtxo.asUtxo)

            val spendUtxosFunding = utxosFunding.toList.map(Spend(_, PubKeyWitness))

            val depositDatum: DepositUtxo.Datum =
                DepositUtxo.Datum(DepositUtxo.Refund.Instructions.Onchain.apply(refundInstructions))

            val depositValue = l2Value + Value(depositFee)

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

            val submissionDeadline = config.txTiming.depositSubmissionEndTime(requestValidityEndTime)
            
            val ttl = ValidityEndSlot(submissionDeadline.toSlot.slot)

            val payloadHash: Hash32 = Hash(blake2b_256(l2Payload))
            val metadata = Some(
              MD(
                MD.Deposit(
                  headAddress = config.headMultisigAddress,
                  depositUtxoIx = 0, // This builder produces the deposit utxo at index 0
                  l2PayloadHash = payloadHash,
                  depositFee = depositFee
                )
              )
            )
            val addRefundMetadata =
                ModifyAuxiliaryData(_ => metadata)

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

                // _ = println(s"!!!!! ---- ${HexUtil.encodeHexString(ctx.transaction.toCbor)}")

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
                  l2Payload = l2Payload,
                  depositFee = depositFee,
                  submissionDeadline = submissionDeadline,
                  absorptionStartTime =
                      config.txTiming.depositAbsorptionStartTime(requestValidityEndTime)
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
            case HashMismatchL2Payload(
                l2Payload: ByteString,
                hash: Hash32
            )
            case MissingDepositOutputAtIndex(e: Int)
            case DepositUtxoError(e: DepositUtxo.DepositUtxoConversionError)
            case TxCborDeserializationFailed(e: Throwable)
            case ValidityEndParseError(e: Throwable)
            case MultisigRegimeWitnessUtxoNotReferenced
            case InvalidDatumContent(e: Throwable)
            case InvalidDatumType
        }
    }

    /** Parse a deposit transaction, ensuring that there is exactly one Babbage Utxo at the head
      * address (given in the transaction metadata) with an Inline datum that parses correctly.
      */
    final case class Parse(config: Config)(
        txBytes: Tx.Serialized,
        l2Payload: ByteString
    ) {
        import Parse.*
        import Parse.Error.*

        def result: ParseErrorOr[DepositTx] = {

            given OriginalCborByteArray = OriginalCborByteArray(txBytes.bytes)
            given ProtocolVersion = config.cardanoProtocolVersion

            io.bullet.borer.Cbor.decode(txBytes.bytes).to[Transaction].valueTry match {
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
                        Metadata.Deposit(headAddress, depositUtxoIx, l2PayloadHash, depositFee) =
                            d

                        // Check head address
                        _ <- Either.cond(
                          headAddress == config.headMultisigAddress,
                          (),
                          AlienDeposit(headAddress)
                        )

                        // Compare hash with virtual outputs
                        calculatedL2PayloadHash: Hash32 = Hash(
                          blake2b_256(l2Payload)
                        )
                        _ <- Either.cond(
                          l2PayloadHash == calculatedL2PayloadHash,
                          (),
                          HashMismatchL2Payload(l2Payload, l2PayloadHash)
                        )

                        // Grab the deposit output at the index specified in the metadata
                        depositOutput <- tx.body.value.outputs
                            .lift(depositUtxoIx)
                            .toRight(MissingDepositOutputAtIndex(depositUtxoIx))

                        // TODO: check: contains ada only
                        l2Value = depositOutput.value.value - Value(depositFee)

                        // Parse the deposit datum
                        depositDatum <- depositOutput.value.datumOption match {
                            case Some(DatumOption.Inline(d)) =>
                                Try(fromData[DepositUtxo.Datum](d)) match {
                                    case Failure(e)  => Left(InvalidDatumContent(e))
                                    case Success(dd) => Right(dd)
                                }
                            case _ => Left(InvalidDatumType)
                        }

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
                              l2Payload = l2Payload,
                              depositFee = depositFee,
                              submissionDeadline = validityEnd,
                              txTiming = config.txTiming
                            )
                            .left
                            .map(DepositUtxoError(_))

                    } yield DepositTx(depositUtxo, validityEnd, tx)
                case Failure(e) => Left(TxCborDeserializationFailed(e))
            }
        }
    }
}

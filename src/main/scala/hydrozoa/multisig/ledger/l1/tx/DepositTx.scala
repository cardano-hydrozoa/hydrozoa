package hydrozoa.multisig.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantizeLosslessUnsafe}
import hydrozoa.lib.cardano.wallet.{Cip30SignedData, Cip30Verify}
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx.Builder.explainConst
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import monocle.{Focus, Lens}
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{fromData, toData}

final case class DepositTx(
    depositProduced: DepositUtxo,
    submissionDeadline: QuantizedInstant,
    override val tx: Transaction,
    override val txLens: Lens[DepositTx, Transaction] = Focus[DepositTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends EnrichedTx[DepositTx] {}

object DepositTx {
    given TxFamily[DepositTx] = TxFamily.of("DepositTx")
    export DepositTxOps.{Build, Parse}
}

private object DepositTxOps {
    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section &
        TxTiming.Section & InitializationParameters.Section & FallbackContingency.Section

    final case class Build(
        utxosFunding: NonEmptyList[Utxo],
        l2Payload: ByteString,
        /** The depositor's CIP-30 COSE endorsement of `blake2b_256(l2Payload)` — carried in the
          * deposit metadata so the head can authenticate the L2 payload (design note §5.5).
          */
        l2PayloadCose: Cip30SignedData,
        l2Value: Value,
        depositFee: Coin,
        changeAddress: ShelleyAddress,
        requestValidityEndTime: RequestValidityEndTime,
        refundInstructions: DepositUtxo.Refund.Instructions
    )(using config: Config) {
        def result: Either[(SomeBuildError, String), DepositTx] = {
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

            val submissionDeadline =
                config.txTiming.depositSubmissionDeadline(requestValidityEndTime)

            val ttl = ValidityEndSlot(submissionDeadline.toSlot.slot)

            val metadata = Some(
              MD.Deposit(
                depositIx = 0, // This builder produces the deposit utxo at index 0
                depositFee = depositFee,
                coseKey = ByteString.fromHex(l2PayloadCose.coseKeyCborHex),
                coseSignature = ByteString.fromHex(l2PayloadCose.coseSignatureCborHex)
              ).asAuxData(config.headId)
            )
            val addRefundMetadata =
                ModifyAuxiliaryData(_ => metadata)

            for {
                ctx <- TransactionBuilder
                    .build(
                      config.network,
                      spendUtxosFunding ++ List(
                        // The deposit must reference the multisig regime utxo. This ensures that the deposit
                        // will roll back if the initialization tx rolls back.
                        config.multisigRegimeUtxo.referenceOutput,
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
                      validators = EnrichedTx.Validators.nonSigningNonValidityChecksValidators
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
                  requestValidityEndTime = requestValidityEndTime,
                  absorptionStartTime =
                      config.txTiming.depositAbsorptionStartTime(requestValidityEndTime),
                  absorptionEndTime =
                      config.txTiming.depositAbsorptionEndTime(requestValidityEndTime)
                )
            } yield DepositTx(
              depositProduced = depositProduced,
              submissionDeadline = submissionDeadline,
              tx = tx,
              resolvedUtxos = finalized.resolvedUtxos
            )
        }
    }

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error extends Throwable {
            case MetadataParseError(e: MD.ParseError)
            case AlienDeposit(headAddress: ShelleyAddress)
            case CoseVerificationFailed(e: Cip30Verify.Error)
            case CoseSignedPayloadMismatch(
                signedPayload: ByteString,
                expectedL2PayloadHash: ByteString
            )
            case MissingDepositOutputAtIndex(e: Int)
            case DepositUtxoError(e: DepositUtxo.DepositUtxoConversionError)
            case TxCborDeserializationFailed(e: Throwable)
            case DepositTxTTLParseError(e: Throwable)
            case IncorrectSubmissionDeadline(actual: QuantizedInstant, expected: QuantizedInstant)
            case MultisigRegimeWitnessUtxoNotReferenced
            case InvalidDatumContent(e: Throwable)
            case InvalidDatumType

            override def toString: String = this match {
                case MetadataParseError(e) =>
                    s"MetadataParseError: $e"
                case AlienDeposit(headAddress) =>
                    s"AlienDeposit: deposit sent to wrong head address: $headAddress"
                case CoseVerificationFailed(e) =>
                    s"CoseVerificationFailed: ${e.message}"
                case CoseSignedPayloadMismatch(signedPayload, expectedL2PayloadHash) =>
                    s"CoseSignedPayloadMismatch: the COSE signature covers ${signedPayload.toHex}, " +
                        s"expected blake2b_256(l2Payload) = ${expectedL2PayloadHash.toHex}"
                case MissingDepositOutputAtIndex(e) =>
                    s"MissingDepositOutputAtIndex: no deposit output found at index $e"
                case DepositUtxoError(e) =>
                    s"DepositUtxoError: $e"
                case TxCborDeserializationFailed(e) =>
                    s"TxCborDeserializationFailed: ${e.getMessage}"
                case DepositTxTTLParseError(e) =>
                    s"DepositTxTTLParseError: ${e.getMessage}"
                case IncorrectSubmissionDeadline(actual, expected) =>
                    s"IncorrectSubmissionDeadline: actual=$actual, expected=$expected"
                case MultisigRegimeWitnessUtxoNotReferenced =>
                    "MultisigRegimeWitnessUtxoNotReferenced: deposit transaction must reference the multisig regime witness UTXO"
                case InvalidDatumContent(e) =>
                    s"InvalidDatumContent: ${e.getMessage}"
                case InvalidDatumType =>
                    "InvalidDatumType: deposit datum must be an inline datum"
            }
        }
    }

    /** Parse a deposit transaction, ensuring that there is exactly one Babbage Utxo at the head
      * address (given in the transaction metadata) with an Inline datum that parses correctly.
      */
    final case class Parse(config: Config)(
        txBytes: EnrichedTx.Serialized,
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
                        mdParseResult <- MD.Deposit.parse(tx).left.map(MetadataParseError(_))
                        (_, md) = mdParseResult
                        Metadata.Deposit(depositUtxoIx, depositFee, coseKey, coseSignature) = md

                        // Verify the depositor's COSE endorsement: the signature must be valid,
                        // and it must cover blake2b_256(l2Payload) — binding the out-of-band L2
                        // payload to this on-chain deposit (design note §5.5).
                        coseVerified <- Cip30Verify
                            .verify(coseKey.toHex, coseSignature.toHex)
                            .left
                            .map(CoseVerificationFailed(_))
                        (_, signedPayload) = coseVerified
                        expectedL2PayloadHash = blake2b_256(l2Payload)
                        _ <- Either.cond(
                          signedPayload == expectedL2PayloadHash,
                          (),
                          CoseSignedPayloadMismatch(signedPayload, expectedL2PayloadHash)
                        )

                        // Grab the deposit output at the index specified in the metadata
                        depositOutput <- tx.body.value.outputs
                            .lift(depositUtxoIx)
                            .toRight(MissingDepositOutputAtIndex(depositUtxoIx))

                        // TODO: check: contains ada only
                        _ = depositOutput.value.value - Value(depositFee)

                        // Parse the deposit datum
                        _ <- depositOutput.value.datumOption match {
                            case Some(DatumOption.Inline(d)) =>
                                Try(fromData[DepositUtxo.Datum](d)) match {
                                    case Failure(e)  => Left(InvalidDatumContent(e))
                                    case Success(dd) => Right(dd)
                                }
                            case _ => Left(InvalidDatumType)
                        }

                        // Read the deposit tx's mandatory TTL and recover its submission deadline
                        // (the tx's validity-interval end). A missing/malformed TTL fails here.
                        submissionDeadline <- Try {
                            val ttlSlot = tx.body.value.ttl.get
                            val ttlPosixMillis = config.slotConfig.slotToTime(ttlSlot)
                            val instant = java.time.Instant.ofEpochMilli(ttlPosixMillis)
                            instant.quantizeLosslessUnsafe(config.slotConfig)
                        } match {
                            case Failure(exception) => Left(DepositTxTTLParseError(exception))
                            case Success(v)         => Right(v)
                        }

                        // Derive the accept-by deadline from the TTL: a deposit's validityEnd sits
                        // submissionDuration below its tx TTL (inverse of
                        // TxTiming.depositSubmissionDeadline).
                        requestValidityEndTime = RequestValidityEndTime(
                          submissionDeadline - config.depositSubmissionDuration.convert
                        )

                        // Check the multisig regime witness utxo was referenced
                        _ <- Either.cond(
                          tx.body.value.referenceInputs.toSet
                              .contains(config.multisigRegimeUtxo.input),
                          (),
                          MultisigRegimeWitnessUtxoNotReferenced
                        )

                        depositUtxo <- DepositUtxo
                            .parseUtxo(
                              utxo =
                                  Utxo(TransactionInput(tx.id, depositUtxoIx), depositOutput.value),
                              headNativeScriptAddress = config.headMultisigAddress,
                              l2Payload = l2Payload,
                              depositFee = depositFee,
                              requestValidityEndTime = requestValidityEndTime,
                              txTiming = config.txTiming
                            )
                            .left
                            .map(DepositUtxoError(_))

                    } yield DepositTx(
                      depositProduced = depositUtxo,
                      submissionDeadline = submissionDeadline,
                      tx = tx,
                      // Parsed from CBOR bytes; ledger context isn't available at parse time.
                      resolvedUtxos = ResolvedUtxos.empty
                    )
                case Failure(e) => Left(TxCborDeserializationFailed(e))
            }
        }
    }
}

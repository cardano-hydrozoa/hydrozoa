package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.token.CIP67.{HasTokenNames, HeadTokenNames}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Initialization
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuilderResultSimple, explainConst}
import hydrozoa.multisig.ledger.dapp.utxo.{Equity, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import monocle.{Focus, Lens}
import scala.util.Try
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}
import scalus.cardano.txbuilder.TxBalancingError.InsufficientFunds
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData

final case class InitializationTx(
    override val validityEnd: QuantizedInstant,
    override val treasuryProduced: MultisigTreasuryUtxo,
    override val multisigRegimeProduced: MultisigRegimeUtxo,
    override val headTokenNames: HeadTokenNames,
    override val resolvedUtxos: ResolvedUtxos,
    override val tx: Transaction,
    override val txLens: Lens[InitializationTx, Transaction] = Focus[InitializationTx](_.tx)
) extends Tx[InitializationTx],
      HasResolvedUtxos,
      MultisigTreasuryUtxo.Produced,
      MultisigRegimeUtxo.Produced,
      HasTokenNames,
      HasValidityEnd

object InitializationTx {
    export InitializationTxOps.{Build, Parse}
}

private object InitializationTxOps {
    type Config = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
        TxTiming.Section & InitializationParameters.Section

    private val logger = org.slf4j.LoggerFactory.getLogger("InitializationTx")

    private def time[A](label: String)(block: => A): A = {
        val start = System.nanoTime()
        val result = block
        val elapsed = (System.nanoTime() - start) / 1_000_000.0
        logger.info(f"\t\t⏱️ $label: ${elapsed}%.2f ms")
        result
    }

    final case class Build(config: Config) {
        import Build.*

        lazy val result: BuilderResultSimple[InitializationTx] = for {
            _ <- Either
                .cond(
                  config.isBalancedInitializationFunding,
                  (),
                  SomeBuildError.BalancingError(
                    TxBalancingError.Failed(new IllegalArgumentException),
                    TransactionBuilder.Context.empty(networkId = config.network)
                  )
                )
                .explainConst(
                  "Initialization tx funding is unbalanced. We must have" +
                      "\n\tconfig.initialFundingValue == config.initialL2Value " +
                      "+ Value(config.initialEquityContributed " +
                      "+ config.totalFallbackContingency)" +
                      ""
                )

            unbalanced <- time("TransactionBuilder.build") {
                TransactionBuilder
                    .build(config.network, Steps())
                    .explainConst("Initialization tx build steps failed.")
            }

            finalized <- time("finalizeContext") {
                TxBuilder
                    .finalizeContext(unbalanced)
                    .explainConst("Initialization tx failed to finalize")
            }

            completed <- Complete(finalized)
        } yield completed

        object Steps {
            def apply(): List[TransactionBuilderStep] =
                Base() ++ Spends() ++ Mints() ++ Sends()

            object Base {
                def apply(): List[TransactionBuilderStep] =
                    List(modifyAuxiliaryData, validityEndSlot)

                private val modifyAuxiliaryData =
                    ModifyAuxiliaryData(_ =>
                        Some(
                          MD.apply(
                            Initialization(
                              headAddress = config.headMultisigAddress,
                              treasuryOutputIndex = 0,
                              multisigRegimeOutputIndex = 1,
                              seedInput = config.initialSeedUtxo.input
                            )
                          )
                        )
                    )

                private[tx] val validityEndTime =
                    config.txTiming.initializationEndTime(config.headStartTime)

                private val validityEndSlot = ValidityEndSlot(validityEndTime.toSlot.slot)
            }

            object Mints {
                def apply(): List[Mint] = List(mintTreasuryToken, mintMultisigRegimeToken)

                private val mintTreasuryToken = Mint(
                  config.headMultisigScript.script.scriptHash,
                  config.headTokenNames.treasuryTokenName,
                  1,
                  config.headMultisigScript.witnessValue
                )

                private val mintMultisigRegimeToken = Mint(
                  config.headMultisigScript.script.scriptHash,
                  config.headTokenNames.multisigRegimeTokenName,
                  1,
                  config.headMultisigScript.witnessAttached
                )
            }

            object Spends {
                def apply(): List[Spend] = config.initialFundingUtxos.iterator
                    .map(kv => Spend(Utxo(kv), PubKeyWitness))
                    .toList
            }

            object Sends {
                def apply(): List[Send] = List(Treasury(), MultisigRegime()) ++ ChangeOutputs()

                object Treasury {
                    def apply(): Send = Send(treasuryOutput)

                    private val treasuryValueUnbalanced: Value =
                        config.initialL2Value + Value(config.initialEquityContributed) +
                            Value.asset(
                              config.headMultisigScript.policyId,
                              config.headTokenNames.treasuryTokenName,
                              1L
                            )

                    private[tx] val treasuryDatum =
                        MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum(
                          config.initialEvacuationMap.cooked
                        )

                    private val treasuryOutput = Babbage(
                      config.headMultisigAddress,
                      treasuryValueUnbalanced,
                      Some(Inline(treasuryDatum.toData))
                    )
                }

                object MultisigRegime {
                    def apply(): Send = Send(multisigRegimeOutput)

                    private val multisigRegimeUtxoValue = Value(config.totalFallbackContingency) +
                        Value.asset(
                          config.headMultisigScript.policyId,
                          config.headTokenNames.multisigRegimeTokenName,
                          1L
                        )

                    private[tx] val multisigRegimeOutput = Babbage(
                      config.headMultisigAddress,
                      multisigRegimeUtxoValue,
                      None,
                      Some(ScriptRef(config.headMultisigScript.script))
                    )
                }

                object ChangeOutputs {
                    def apply(): List[Send] = config.initialChangeOutputs.map(Send.apply)
                }
            }

        }

        private object Complete {
            def apply(
                finalized: TransactionBuilder.Context
            ): BuilderResultSimple[InitializationTx] =
                val treasuryIndex = 0
                val multisigRegimeIndex = 1

                val treasuryValue =
                    finalized.transaction.body.value.outputs(treasuryIndex).value.value

                val multisigRegimeProduced = MultisigRegimeUtxo(
                  config.headTokenNames.multisigRegimeTokenName,
                  utxoId = TransactionInput(finalized.transaction.id, multisigRegimeIndex),
                  output = Steps.Sends.MultisigRegime.multisigRegimeOutput,
                  script = config.headMultisigScript
                )

                val equityCoin =
                    config.initialEquityContributed - finalized.transaction.body.value.fee

                for {
                    equity <- Equity(equityCoin)
                        .toRight(
                          SomeBuildError.BalancingError(
                            InsufficientFunds(
                              Value(equityCoin),
                              finalized.transaction.body.value.fee.value
                            ),
                            finalized
                          )
                        )
                        .explainConst(
                          s"There is not enough equity (${config.initialEquityContributed}) to pay for the" +
                              s" initialization tx fee of ${finalized.transaction.body.value.fee}"
                        )
                    treasuryProduced = MultisigTreasuryUtxo(
                      treasuryTokenName = config.headTokenNames.treasuryTokenName,
                      utxoId = TransactionInput(finalized.transaction.id, treasuryIndex),
                      address = config.headMultisigAddress,
                      datum = Steps.Sends.Treasury.treasuryDatum,
                      value = treasuryValue,
                      equity = equity
                    )
                } yield InitializationTx(
                  validityEnd = Steps.Base.validityEndTime,
                  treasuryProduced = treasuryProduced,
                  tx = finalized.transaction,
                  multisigRegimeProduced = multisigRegimeProduced,
                  headTokenNames = config.headTokenNames,
                  resolvedUtxos = finalized.resolvedUtxos
                )
        }

        private object TxBuilder {
            private val diffHandler = Change.changeOutputDiffHandler(
              _,
              _,
              protocolParams = config.cardanoProtocolParams,
              changeOutputIdx = 0
            )

            def finalizeContext(
                ctx: TransactionBuilder.Context
            ): Either[SomeBuildError, TransactionBuilder.Context] =
                ctx.finalizeContext(
                  config.cardanoProtocolParams,
                  diffHandler = diffHandler,
                  evaluator = config.plutusScriptEvaluatorForTxBuild,
                  validators = Tx.Validators.nonSigningNonValidityChecksValidators
                )
        }
    }

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error extends Throwable {
            case MetadataParseError(wrapped: MD.ParseError)
            case InvalidTransactionError(msg: String)
            case TtlIsMissing
            case InvalidInitializationTtl
            case EquityToLow(equity: Coin, fee: Coin)
        }

    }

    final case class Parse(config: Config)(
        tx: Transaction,
        resolvedUtxos: ResolvedUtxos
    ) {
        import Parse.*
        import Error.*

        private given ProtocolVersion = config.cardanoProtocolVersion

        def result: ParseErrorOr[InitializationTx] = for {
            // ===================================
            // Metadata parsing
            // ===================================
            imd <- MD.parse(tx) match {
                case Right(md: Initialization) => Right(md)
                case Right(md) =>
                    Left(
                      MetadataParseError(
                        MD.UnexpectedTxType(actual = md, expected = "Initialization")
                      )
                    )
                case Left(e) =>
                    Left(
                      MetadataParseError(
                        MD.MalformedTxTypeKey(
                          "Could not find the expected TxTypeKey for Initialization" +
                              s" transaction. Got: $e"
                        )
                      )
                    )
            }

            // ===================================
            // Data Extraction
            // ===================================

            expectedHNS = config.headMultisigScript

            expectedHeadAddress = config.headMultisigAddress

            expectedEndTime = config.txTiming.initializationEndTime(config.headStartTime)

            expectedTreasuryDatum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum(
              config.initialEvacuationMap.cooked
            )

            actualOutputs = tx.body.value.outputs.map(_.value)

            actualTreasuryOutput = actualOutputs(imd.treasuryOutputIndex)
            actualMultisigRegimeOutput = actualOutputs(imd.multisigRegimeOutputIndex)

            mbTtl = tx.body.value.ttl

            // ===================================
            // Validation
            // ===================================

            ////
            // Head address is coherent
            _ <-
                if expectedHeadAddress == imd.headAddress
                then Right(())
                else Left(InvalidTransactionError("Invalid head address"))

            // Seed input is coherent
            _ <-
                if tx.body.value.inputs.toSeq.contains(imd.seedInput)
                then Right(())
                else Left(InvalidTransactionError("Seed input missing"))

            /////
            // Treasury is coherent: address matches, contains head token, datum is initial treasury datum,
            // no reference script

            // address
            _ <-
                if actualTreasuryOutput.address == expectedHeadAddress
                then Right(())
                else Left(InvalidTransactionError("Unexpected treasury address"))
            // value
            treasuryOutputInner <- actualTreasuryOutput.value.assets.assets
                .get(expectedHNS.policyId)
                .toRight(
                  InvalidTransactionError(
                    "Head Native Script policy ID not found in treasury output value"
                  )
                )
            _ <- treasuryOutputInner.get(config.headTokenNames.treasuryTokenName) match {
                case None =>
                    Left(
                      InvalidTransactionError(
                        "No tokens matching the head token asset name found in treasury output"
                      )
                    )
                case Some(1L) => Right(())
                case Some(wrongCount) =>
                    Left(
                      InvalidTransactionError(
                        "Multiple tokens matching the head token asset" +
                            " name found in the treasury ouptu"
                      )
                    )
            }
            // datum
            encodedTreasuryDatum <- actualTreasuryOutput.datumOption match {
                case None => Left(InvalidTransactionError("treasury output datum missing"))
                case Some(Inline(i)) => Right(i)
                case Some(_) => Left(InvalidTransactionError("treasury output datum not inline"))
            }
            decodedTreasuryDatum <- Try(
              Data.fromData[MultisigTreasuryUtxo.Datum](encodedTreasuryDatum)
            ).toEither.left
                .map(_ => InvalidTransactionError("data decoding of treasury datum failed"))
            _ <-
                if decodedTreasuryDatum == expectedTreasuryDatum then Right(())
                else
                    Left(
                      InvalidTransactionError(
                        "actual treasury datum does not match the expected initial " +
                            "treasury datum"
                      )
                    )

            // script
            _ <-
                if actualTreasuryOutput.scriptRef.isEmpty then Right(())
                else
                    Left(
                      InvalidTransactionError("treasury output has reference script, but shouldn't")
                    )

            //////
            // Multisig regime is coherent: expected address, contains only MR token and ADA, datum is None, HNS in
            // reference script

            // address
            _ <-
                if actualMultisigRegimeOutput.address == expectedHeadAddress
                then Right(())
                else Left(InvalidTransactionError("Multisig regime output has the wrong address"))

            // value
            mrValueOuter <- actualMultisigRegimeOutput.value.assets.assets
                .get(expectedHNS.policyId)
                .toRight(
                  InvalidTransactionError(
                    "value of the multisig regime output" +
                        "does not contain the head native script policyId"
                  )
                )
            _ <- mrValueOuter.get(config.headTokenNames.multisigRegimeTokenName) match {
                case None =>
                    Left(
                      InvalidTransactionError(
                        "No tokens found matching the multisig regime token name"
                      )
                    )
                case Some(1L) => Right(())
                case Some(_) =>
                    Left(
                      InvalidTransactionError(
                        "Multiple tokens found matching the" +
                            " multisig regime token name"
                      )
                    )
            }

            // datum
            _ <-
                if actualMultisigRegimeOutput.datumOption.isEmpty then Right(())
                else
                    Left(
                      InvalidTransactionError("multisig witness utxo has a non-empty datum")
                    )

            _ <-
                if actualMultisigRegimeOutput.scriptRef.contains(
                      ScriptRef.apply(expectedHNS.script)
                    )
                then Right(())
                else
                    Left(
                      InvalidTransactionError(
                        "Multisig regime witness UTxO does not contain the expected head" +
                            "native script"
                      )
                    )

            // ttl should be present
            validityEndSlot <- mbTtl.toRight(TtlIsMissing)

            // Should this be in the init tx parser?
            _ <- Either.cond(
              test = expectedEndTime.toSlot == Slot(validityEndSlot),
              right = (),
              left = InvalidInitializationTtl
            )

            //////
            // Check mint coherence: only a single head token and MR token should be minted
            mintOuter <- tx.body.value.mint.toRight(InvalidTransactionError("Mints are empty"))
            mintInner <- mintOuter.assets
                .get(expectedHNS.policyId)
                .toRight(InvalidTransactionError("Mints don't contain the HNS policy id"))
            _ <- mintInner.get(config.headTokenNames.treasuryTokenName) match {
                case None     => Left(InvalidTransactionError("head token not minted"))
                case Some(1L) => Right(())
                case Some(wrongNumber) =>
                    Left(InvalidTransactionError("multiple head tokens minted"))
            }
            _ <- mintInner.get(config.headTokenNames.multisigRegimeTokenName) match {
                case None              => Left(InvalidTransactionError("MR token not minted"))
                case Some(1L)          => Right(())
                case Some(wrongNumber) => Left(InvalidTransactionError("multiple MR tokens minted"))
            }

            equity <- Equity(config.initialEquityContributed - tx.body.value.fee)
                .toRight(EquityToLow(config.initialEquityContributed, tx.body.value.fee))

            treasury = MultisigTreasuryUtxo(
              treasuryTokenName = config.headTokenNames.treasuryTokenName,
              utxoId = TransactionInput(tx.id, imd.treasuryOutputIndex),
              address = expectedHeadAddress,
              datum = expectedTreasuryDatum,
              value = actualTreasuryOutput.value,
              equity = equity
            )

            multisigRegimeWitness = Utxo(
              TransactionInput(tx.id, imd.multisigRegimeOutputIndex),
              actualMultisigRegimeOutput
            )

        } yield InitializationTx(
          validityEnd = QuantizedInstant(
            config.slotConfig,
            java.time.Instant.ofEpochMilli(config.slotConfig.slotToTime(validityEndSlot))
          ),
          treasuryProduced = treasury,
          multisigRegimeProduced = MultisigRegimeUtxo(
            multisigRegimeTokenName = config.headTokenNames.multisigRegimeTokenName,
            utxoId = TransactionInput(tx.id, imd.multisigRegimeOutputIndex),
            address = expectedHeadAddress,
            value = actualMultisigRegimeOutput.value,
            script = expectedHNS
          ),
          headTokenNames = config.headTokenNames,
          resolvedUtxos = resolvedUtxos,
          tx = tx
        )
    }
}

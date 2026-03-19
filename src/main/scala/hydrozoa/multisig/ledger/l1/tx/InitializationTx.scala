package hydrozoa.multisig.ledger.l1.tx

import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, InitializationTxEndTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.token.CIP67.{HasTokenNames, HeadTokenNames}
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.tx.Metadata.Initialization
import hydrozoa.multisig.ledger.l1.tx.Tx.Builder.{BuilderResultSimple, explainConst}
import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigRegimeUtxo, MultisigTreasuryUtxo}
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
    initializationTxEndTime: InitializationTxEndTime,
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
      HasTokenNames

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
        logger.trace(f"\t\t⏱️ $label: ${elapsed}%.2f ms")
        result
    }

    final case class Build(config: Config)(blockCreationEndTime: BlockCreationEndTime) {
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
                          Initialization(
                            multisigTreasuryIx = 0,
                            multisigRegimeIx = 1,
                            seedIx = config.initialSeedIx
                          ).asAuxData(config.headId)
                        )
                    )

                private[tx] val initializationTxEndTime: InitializationTxEndTime =
                    config.txTiming.initializationEndTime(blockCreationEndTime)

                private val validityEndSlot = ValidityEndSlot(initializationTxEndTime.toSlot.slot)
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
                          config.initialEvacuationMap
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
                  initializationTxEndTime = Steps.Base.initializationTxEndTime,
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

            override def toString: String = this match {
                case MetadataParseError(wrapped) =>
                    s"MetadataParseError: $wrapped"
                case InvalidTransactionError(msg) =>
                    s"InvalidTransactionError: $msg"
                case TtlIsMissing =>
                    "TtlIsMissing: initialization transaction must have a TTL"
                case InvalidInitializationTtl =>
                    "InvalidInitializationTtl: initialization transaction TTL is invalid"
                case EquityToLow(equity, fee) =>
                    s"EquityToLow: equity ($equity) is too low to cover fee ($fee)"
            }

        }

    }

    final case class Parse(config: Config)(
        blockCreationEndTime: BlockCreationEndTime,
        tx: Transaction,
        resolvedUtxos: ResolvedUtxos
    ) {
        import Parse.*
        import Error.*

        def result: ParseErrorOr[InitializationTx] = for {
            // ===================================
            // Metadata parsing
            // ===================================
            mdParseResult <- MD.Initialization.parse(tx).left.map(MetadataParseError(_))
            (head, md) = mdParseResult

            // ===================================
            // Data Extraction
            // ===================================

            expectedHNS = config.headMultisigScript

            expectedHeadAddress = config.headMultisigAddress

            initializationTxEndTime = config.txTiming.initializationEndTime(blockCreationEndTime)

            expectedTreasuryDatum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum(
              config.initialEvacuationMap
            )

            actualOutputs = tx.body.value.outputs.map(_.value)

            actualTreasuryOutput <- Try(actualOutputs(md.multisigTreasuryIx)).toEither.left.map(_ =>
                InvalidTransactionError(
                  "Multisig treasury index given as" +
                      s" ${md.multisigTreasuryIx}, but there are only ${actualOutputs.length} outputs"
                )
            )
            actualMultisigRegimeOutput <- Try(actualOutputs(md.multisigRegimeIx)).toEither.left.map(
              _ =>
                  InvalidTransactionError(
                    "Multisig regime index given as" +
                        s" ${md.multisigTreasuryIx}, but there are only ${actualOutputs.length} outputs"
                  )
            )

            mbTtl = tx.body.value.ttl

            // ===================================
            // Validation
            // ===================================

            // Seed input is coherent
            seedInput <- Try(tx.body.value.inputs).toEither.left.map(_ =>
                InvalidTransactionError(
                  s"Seed index given as ${md.seedIx}, " +
                      s"but there are only ${tx.body.value.inputs.toSeq.size} inputs"
                )
            )

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
                            " name found in the treasury output"
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
            _ <-
                if actualMultisigRegimeOutput.value ==
                        Value(
                          config.totalFallbackContingency,
                          MultiAsset.asset(
                            expectedHNS.policyId,
                            config.headTokenNames.multisigRegimeTokenName,
                            1
                          )
                        )
                then Right(())
                else Left(InvalidTransactionError("multisig regime output has wrong value"))

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
            ttl <- mbTtl.toRight(TtlIsMissing)

            // Should this be in the init tx parser?
            _ <- Either.cond(
              test = initializationTxEndTime.toSlot.slot == ttl,
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
              utxoId = TransactionInput(tx.id, md.multisigTreasuryIx),
              address = expectedHeadAddress,
              datum = expectedTreasuryDatum,
              value = actualTreasuryOutput.value,
              equity = equity
            )

            multisigRegimeWitness = Utxo(
              TransactionInput(tx.id, md.multisigRegimeIx),
              actualMultisigRegimeOutput
            )

        } yield InitializationTx(
          initializationTxEndTime = initializationTxEndTime,
          treasuryProduced = treasury,
          multisigRegimeProduced = MultisigRegimeUtxo(
            multisigRegimeTokenName = config.headTokenNames.multisigRegimeTokenName,
            utxoId = TransactionInput(tx.id, md.multisigRegimeIx),
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

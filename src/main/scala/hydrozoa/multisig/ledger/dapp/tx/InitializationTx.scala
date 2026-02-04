package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.{Utxo as _, *}
import monocle.{Focus, Lens}
import scala.util.Try
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}

final case class InitializationTx(
    override val validityEnd: QuantizedInstant,
    treasuryProduced: MultisigTreasuryUtxo,
    multisigRegimeUtxo: MultisigRegimeUtxo,
    tokenNames: HeadTokenNames,
    override val resolvedUtxos: ResolvedUtxos,
    override val tx: Transaction,
    override val txLens: Lens[InitializationTx, Transaction] = Focus[InitializationTx](_.tx)
) extends Tx[InitializationTx],
      HasResolvedUtxos,
      HasValidityEnd

object InitializationTx {
    type Config = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
        TxTiming.Section & InitializationParameters.Section

    def build(config: Config): Either[SomeBuildError, InitializationTx] = {
        /////////////////////////////////////////////////////////
        // Base steps
        val modifyAuxiliaryData =
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

        val validityEndTime = config.txTiming.initializationEndTime(config.headStartTime)

        val validityEndSlot = ValidityEndSlot(validityEndTime.toSlot.slot)

        val baseSteps = List(modifyAuxiliaryData, validityEndSlot)

        /////////////////////////////////////////////////////////
        // Mints
        val mintTreasuryToken = Mint(
          config.headMultisigScript.script.scriptHash,
          config.headTokenNames.treasuryTokenName,
          1,
          config.headMultisigScript.witnessValue
        )

        val mintMultisigRegimeToken = Mint(
          config.headMultisigScript.script.scriptHash,
          config.headTokenNames.multisigRegimeTokenName,
          1,
          config.headMultisigScript.witnessAttached
        )

        val mintSteps = List(mintTreasuryToken, mintMultisigRegimeToken)

        /////////////////////////////////////////////////////////
        // Spend steps
        val spendSteps: List[Spend] =
            config.initialFundingUtxos.iterator.map(kv => Spend(Utxo(kv), PubKeyWitness)).toList

        ////////////////////////////////////////////////////////////
        // Output values

        /////////////////////////////////////////////////////////
        // Send steps
        val treasuryValueUnbalanced: Value =
            config.initialL2Value + Value(config.initialEquityContributed) +
                Value.asset(
                  config.headMultisigScript.policyId,
                  config.headTokenNames.treasuryTokenName,
                  1L
                )

        val multisigRegimeUtxoValue = Value(config.totalFallbackContingency) +
            Value.asset(
              config.headMultisigScript.policyId,
              config.headTokenNames.multisigRegimeTokenName,
              1L
            )

        val treasuryDatum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum(config.initialL2Utxos)

        val treasuryOutput = Babbage(
          config.headMultisigAddress,
          treasuryValueUnbalanced,
          Some(Inline(treasuryDatum.toData))
        )

        val multisigRegimeOutput = Babbage(
          config.headMultisigAddress,
          multisigRegimeUtxoValue,
          None,
          Some(ScriptRef(config.headMultisigScript.script))
        ).ensureMinAda(config.cardanoInfo.protocolParams)

        val createTreasury: Send = Send(treasuryOutput)

        val createMultisigRegimeOutput = Send(multisigRegimeOutput)

        val createChangeOutputs: List[Send] = config.initialChangeOutputs.map(Send.apply)

        val sendSteps = List(createTreasury, createMultisigRegimeOutput) ++ createChangeOutputs

        ////////////////////////////////////////////////////////////
        // Build and finalize
        val steps = baseSteps ++ spendSteps ++ mintSteps ++ sendSteps

        for {
            _ <- Either.cond(
              config.isBalancedInitializationFunding,
              (),
              ???
            ) // FIXME: provide an error
            unbalanced <- TransactionBuilder
                .build(
                  config.cardanoInfo.network,
                  steps
                )

            finalized <- unbalanced
                .finalizeContext(
                  protocolParams = config.cardanoInfo.protocolParams,
                  diffHandler =
                      Change.changeOutputDiffHandler(_, _, config.cardanoInfo.protocolParams, 0),
                  evaluator = config.plutusScriptEvaluatorForTxBuild,
                  validators = Tx.Validators.nonSigningNonValidityChecksValidators
                )

        } yield InitializationTx(
          validityEnd = validityEndTime,
          treasuryProduced = MultisigTreasuryUtxo(
            treasuryTokenName = config.headTokenNames.treasuryTokenName,
            utxoId = TransactionInput(
              transactionId = finalized.transaction.id,
              index = 0
            ),
            address = config.headMultisigAddress,
            datum = treasuryDatum,
            value = finalized.transaction.body.value.outputs(0).value.value
          ),
          tx = finalized.transaction,
          multisigRegimeUtxo = MultisigRegimeUtxo(
            config.headTokenNames.multisigRegimeTokenName,
            utxoId = TransactionInput(finalized.transaction.id, 1),
            output = multisigRegimeOutput,
            script = config.headMultisigScript
          ),
          tokenNames = config.headTokenNames,
          resolvedUtxos = finalized.resolvedUtxos
        )
    }

    // TODO: use validation monad instead of Either?
    def parse(
        config: Config,
        tx: Transaction,
        // FIXME: We need to parse that these are actually satisfactory, I guess?
        resolvedUtxos: ResolvedUtxos
    )(using protocolVersion: ProtocolVersion): Either[ParseError, InitializationTx] =
        for {
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
              config.initialL2Utxos
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
                if decodedTreasuryDatum == MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum then
                    Right(())
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

            // script
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

            treasury = MultisigTreasuryUtxo(
              treasuryTokenName = config.headTokenNames.treasuryTokenName,
              utxoId = TransactionInput(tx.id, imd.treasuryOutputIndex),
              address = expectedHeadAddress,
              datum = expectedTreasuryDatum,
              value = actualTreasuryOutput.value
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
          multisigRegimeUtxo = MultisigRegimeUtxo(
            multisigRegimeTokenName = config.headTokenNames.multisigRegimeTokenName,
            utxoId = TransactionInput(tx.id, imd.multisigRegimeOutputIndex),
            address = expectedHeadAddress,
            value = actualMultisigRegimeOutput.value,
            script = expectedHNS
          ),
          tokenNames = config.headTokenNames,
          resolvedUtxos = resolvedUtxos,
          tx = tx
        )

    sealed trait ParseError extends Throwable

    case class MetadataParseError(wrapped: MD.ParseError) extends ParseError

    case class InvalidTransactionError(msg: String) extends ParseError

    case object TtlIsMissing extends ParseError

    case object InvalidInitializationTtl extends ParseError
}

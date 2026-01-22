package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Initialization
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.*
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.{Utxo as _, *}
import monocle.{Focus, Lens}
import scala.collection.immutable.SortedMap
import scala.util.Try
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}

final case class InitializationTx(
    override val validityEnd: QuantizedInstant,
    treasuryProduced: MultisigTreasuryUtxo,
    multisigRegimeUtxo: MultisigRegimeUtxo,
    tokenNames: TokenNames,
    override val resolvedUtxos: ResolvedUtxos,
    override val tx: Transaction,
    override val txLens: Lens[InitializationTx, Transaction] = Focus[InitializationTx](_.tx)
) extends Tx[InitializationTx],
      HasResolvedUtxos,
      HasValidityEnd

object InitializationTx {
    case class Config(
        peerKeys: NonEmptyList[VerificationKeyBytes],
        cardanoInfo: CardanoInfo,
        txTiming: TxTiming,
        startTime: QuantizedInstant,
        tokenNames: TokenNames
    ) {
        def evaluator: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluateAndComputeCost)
        def headMultisigScript: HeadMultisigScript = HeadMultisigScript(peerKeys)

    }

    def build(config: Config, recipe: Recipe): Either[SomeBuildError, InitializationTx] = {
        ////////////////////////////////////////////////////////////
        // Data extraction
        import recipe.*

        val headAddress: ShelleyAddress =
            config.headMultisigScript.mkAddress(config.cardanoInfo.network)
        val changeAddress = ShelleyAddress(config.cardanoInfo.network, changePP, Null)

        val mrTokenName = CIP67.TokenNames(recipe.spentUtxos.seedUtxo.input).multisigRegimeTokenName
        val mrToken: MultiAsset = MultiAsset(
          SortedMap(
            config.headMultisigScript.policyId -> SortedMap(mrTokenName -> 1L)
          )
        )

        // Lovelace per tx byte (a): 44 Lovelace per tx (b): 155381 Max tx bytes: 16 * 1024 = 16384
        // Therefore, max non-Plutus tx fee: 16 * 1024 * 44 + 155381 = 720896 + 155381 = 876277
        // (this is the deposit to cover the fallback transaction fee)
        val mrValue = Value(recipe.hmrwCoin, mrToken)

        /////////////////////////////////////////////////////////
        // Steps
        val spendAllUtxos: Seq[Spend] = Seq(Spend(recipe.spentUtxos.seedUtxo)) ++
            recipe.spentUtxos.fundingUtxos
                .map(utxo =>
                    Spend(
                      Utxo.apply(utxo._1, utxo._2),
                      PubKeyWitness
                    )
                )
                .toList

        val mintTreasuryToken = Mint(
          config.headMultisigScript.script.scriptHash,
          config.tokenNames.headTokenName,
          1,
          config.headMultisigScript.witnessValue
        )

        val mintMRToken = Mint(
          config.headMultisigScript.script.scriptHash,
          mrTokenName,
          1,
          config.headMultisigScript.witnessAttached
        )
        val hmrwOutput =
            Babbage(headAddress, mrValue, None, Some(ScriptRef(config.headMultisigScript.script)))
                .ensureMinAda(config.cardanoInfo.protocolParams)

        val createTreasury: Send = Send(
          Babbage(
            config.headMultisigScript.mkAddress(config.cardanoInfo.network),
            value = initialTreasury +
                Value(
                  Coin.zero,
                  MultiAsset(
                    SortedMap(
                      config.headMultisigScript.policyId -> SortedMap(
                        config.tokenNames.headTokenName -> 1L
                      )
                    )
                  )
                ),
            datumOption = Some(Inline(MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum.toData))
          )
        )

        val createChangeOutput = Send(Babbage(changeAddress, Value.zero, None, None))

        val modifyAuxiliaryData =
            ModifyAuxiliaryData(_ =>
                Some(
                  MD.apply(
                    Initialization(
                      headAddress = headAddress,
                      treasuryOutputIndex = 0,
                      multisigRegimeOutputIndex = 1,
                      seedInput = recipe.spentUtxos.seedUtxo.input
                    )
                  )
                )
            )

        // Not sure why we use Long in the builder step not Slot
        val validityEndSlot = ValidityEndSlot(
          (config.startTime
              + config.txTiming.minSettlementDuration + config.txTiming.inactivityMarginDuration).toSlot.slot
        )

        val steps = spendAllUtxos
            :+ mintTreasuryToken
            :+ mintMRToken
            :+ createTreasury
            :+ Send(hmrwOutput)
            :+ createChangeOutput
            :+ modifyAuxiliaryData
            :+ validityEndSlot

        ////////////////////////////////////////////////////////////
        // Build and finalize
        for {
            unbalanced <- TransactionBuilder
                .build(
                  config.cardanoInfo.network,
                  steps
                )

            finalized <- unbalanced
                .finalizeContext(
                  protocolParams = config.cardanoInfo.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    config.cardanoInfo.protocolParams,
                    2
                  ).changeOutputDiffHandler,
                  evaluator = config.evaluator,
                  validators = Tx.Validators.nonSigningNonValidityChecksValidators
                )

        } yield InitializationTx(
          validityEnd =
              config.startTime + config.txTiming.minSettlementDuration + config.txTiming.inactivityMarginDuration,
          treasuryProduced = MultisigTreasuryUtxo(
            treasuryTokenName = config.tokenNames.headTokenName,
            utxoId = TransactionInput(
              transactionId = finalized.transaction.id,
              index = 0
            ),
            address = headAddress,
            datum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum,
            value = createTreasury.output.value
          ),
          tx = finalized.transaction,
          multisigRegimeUtxo = MultisigRegimeUtxo(
            config.tokenNames.multisigRegimeTokenName,
            utxoId = TransactionInput(finalized.transaction.id, 1),
            output = hmrwOutput,
            script = config.headMultisigScript
          ),
          tokenNames = config.tokenNames,
          resolvedUtxos = finalized.resolvedUtxos
        )
    }

    // TODO: use validation monad instead of Either?
    def parse(
        peerKeys: NonEmptyList[VerificationKeyBytes],
        cardanoInfo: CardanoInfo,
        txTiming: TxTiming,
        startTime: QuantizedInstant,
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

            derivedTokenNames = CIP67.TokenNames(imd.seedInput)

            expectedHNS = HeadMultisigScript(peerKeys)

            expectedHeadAddress = expectedHNS.mkAddress(cardanoInfo.network)

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
            _ <- treasuryOutputInner.get(derivedTokenNames.headTokenName) match {
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
            _ <- mrValueOuter.get(derivedTokenNames.multisigRegimeTokenName) match {
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
            validityEndSlot <- mbTtl
                .toRight(TtlIsMissing)

            // Should this be in the init tx parser?
            _ <- Either.cond(
              test = (startTime
                  + txTiming.minSettlementDuration
                  + txTiming.inactivityMarginDuration).toSlot
                  == Slot(validityEndSlot),
              right = (),
              left = InvalidInitializationTtl
            )

            //////
            // Check mint coherence: only a single head token and MR token should be minted
            mintOuter <- tx.body.value.mint.toRight(InvalidTransactionError("Mints are empty"))
            mintInner <- mintOuter.assets
                .get(expectedHNS.policyId)
                .toRight(InvalidTransactionError("Mints don't contain the HNS policy id"))
            _ <- mintInner.get(derivedTokenNames.headTokenName) match {
                case None     => Left(InvalidTransactionError("head token not minted"))
                case Some(1L) => Right(())
                case Some(wrongNumber) =>
                    Left(InvalidTransactionError("multiple head tokens minted"))
            }
            _ <- mintInner.get(derivedTokenNames.multisigRegimeTokenName) match {
                case None              => Left(InvalidTransactionError("MR token not minted"))
                case Some(1L)          => Right(())
                case Some(wrongNumber) => Left(InvalidTransactionError("multiple MR tokens minted"))
            }

            treasury = MultisigTreasuryUtxo(
              treasuryTokenName = derivedTokenNames.headTokenName,
              utxoId = TransactionInput(tx.id, imd.treasuryOutputIndex),
              address = expectedHeadAddress,
              datum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum,
              value = actualTreasuryOutput.value
            )

            multisigRegimeWitness = Utxo(
              TransactionInput(tx.id, imd.multisigRegimeOutputIndex),
              actualMultisigRegimeOutput
            )

        } yield InitializationTx(
          validityEnd = QuantizedInstant(
            cardanoInfo.slotConfig,
            java.time.Instant.ofEpochMilli(cardanoInfo.slotConfig.slotToTime(validityEndSlot))
          ),
          treasuryProduced = treasury,
          multisigRegimeUtxo = MultisigRegimeUtxo(
            multisigRegimeTokenName = derivedTokenNames.multisigRegimeTokenName,
            utxoId = TransactionInput(tx.id, imd.multisigRegimeOutputIndex),
            address = expectedHeadAddress,
            value = actualMultisigRegimeOutput.value,
            script = expectedHNS
          ),
          tokenNames = derivedTokenNames,
          resolvedUtxos = resolvedUtxos,
          tx = tx
        )

    sealed trait ParseError extends Throwable

    case class MetadataParseError(wrapped: MD.ParseError) extends ParseError

    case class InvalidTransactionError(msg: String) extends ParseError

    case object TtlIsMissing extends ParseError

    case object InvalidInitializationTtl extends ParseError

    // TODO: rename to Args for consistency?
    final case class Recipe(
        spentUtxos: SpentUtxos,
        initialTreasury: Value,
        hmrwCoin: Coin,
        changePP: ShelleyPaymentPart,
    ) {}

    final case class SpentUtxos(
        seedUtxo: Utxo,
        fundingUtxos: List[Utxo]
    ) {
        def all: NonEmptyList[Utxo] = NonEmptyList(seedUtxo, fundingUtxos)
    }
}

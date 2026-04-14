package hydrozoa.config.head

import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.head.rulebased.scripts.RuleBasedScriptAddresses
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.lib.logging.Logging
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import io.circe.Decoder.Result
import io.circe.generic.semiauto.*
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, *}
import scala.collection.immutable.SortedSet
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.platform

final case class HeadConfig private (
    override val headConfigPreinit: HeadConfig.Preinit,
    override val initialBlock: Block.MultiSigned.Initial,
) extends HeadConfig.Section {
    override transparent inline def headConfig: HeadConfig = this
}

object HeadConfig {

    given headConfigEncoder: Encoder[HeadConfig] with {
        override def apply(hc: HeadConfig): Json = {
            given HeadConfig.Section = hc

            deriveEncoder[HeadConfig](hc)
        }
    }

    given headConfigDecoder: Decoder[HeadConfig] = Decoder.instance { c =>
        for {
            network <- c
                .downField("headConfigPreinit")
                .downField("cardanoNetwork")
                .as[CardanoNetwork]
            preinit <- {
                given CardanoNetwork = network

                c.downField("headConfigPreinit").as[HeadConfig.Preinit]
            }
            hc <- {
                given HeadConfig.Preinit.Section = preinit

                for {
                    brief <- c
                        .downField("initialBlock")
                        .downField("blockBrief")
                        .as[BlockBrief.Initial]
                    initTx <- c
                        .downField("initialBlock")
                        .downField("effects")
                        .downField("initializationTx")
                        .as[Transaction]
                    fallbackTx <- c
                        .downField("initialBlock")
                        .downField("effects")
                        .downField("fallbackTx")
                        .as[Transaction]
                    hc <- HeadConfig(preinit, brief, initTx, fallbackTx).toEither.left.map(_ =>
                        io.circe.DecodingFailure("Failed constructing head config", c.history)
                    )
                } yield hc
            }
        } yield hc
    }

    private val logger = Logging.logger("HeadConfig")

    def apply(
        headConfigPreinit: HeadConfig.Preinit,
        blockBrief: BlockBrief.Initial,
        initTx: Transaction,
        fallbackTx: Transaction
    ): ValidatedNel[String, HeadConfig] = {
        def isMultiSigned(
            tx: Transaction,
            headPeers: HeadPeers.Section
        ): ValidatedNel[String, Unit] = {
            val witnesses = tx.witnessSetRaw.value.vkeyWitnesses.toSet

            headPeers.headPeerVKeys
                .map(vKey =>
                    witnesses.find(witness => witness.vkey == vKey) match {
                        case None =>
                            Invalid(
                              s"Missing multisig witness for verification key $vKey in tx ${tx.id}"
                            )
                        case Some(witness)
                            if platform.verifyEd25519Signature(vKey, tx.id, witness.signature) =>
                            Valid(())
                        case _ =>
                            Invalid(
                              s"Invalid multisig witness for verification key $vKey in tx ${tx.id}"
                            )

                    }
                )
                .foldLeft(Valid(()): ValidatedNel[String, Unit])((acc, v) =>
                    acc.combine(v.leftMap(NonEmptyList.one))
                )
        }

        val validatedInitTxSeq = Validated.fromEither(
          InitializationTxSeq
              .Build(headConfigPreinit)(blockBrief.endTime)
              .result
              .left
              .map(error => NonEmptyList.one(error.getMessage))
        )

        validatedInitTxSeq.andThen(expectedTxSeq => {
            // Check that tx bodies are the same
            val validatedInitTx =
                Validated
                    .cond(
                      test = expectedTxSeq.initializationTx.tx.body == initTx.body,
                      e = NonEmptyList.one(
                        "initialization tx body mismatch when constructing preinit config"
                      ),
                      a = ()
                    )
                    .combine(isMultiSigned(initTx, headConfigPreinit))

            val validatedFallbackTx =
                Validated
                    .cond(
                      expectedTxSeq.fallbackTx.tx.body == fallbackTx.body,
                      e = NonEmptyList.one(
                        "Fallback tx body mismatch when constructing preinit config"
                      ),
                      a = ()
                    )
                    .combine(isMultiSigned(fallbackTx, headConfigPreinit))

            validatedInitTx.combine(validatedFallbackTx) match {
                case Valid(_) =>
                    Valid(
                      new HeadConfig(
                        headConfigPreinit,
                        Block.MultiSigned.Initial(
                          blockBrief,
                          // The "expectedTxSeq" contains the "enriched" tx types, but they are not signed,
                          // so we steal the tx signatures here.
                          BlockEffects.MultiSigned.Initial(
                            expectedTxSeq.initializationTx.copy(tx = initTx),
                            expectedTxSeq.fallbackTx.copy(tx = fallbackTx)
                          )
                        )
                      )
                    )
                case Invalid(errors: NonEmptyList[String]) =>
                    // We log in the constructor rather than the pattern match. If this causes spurious errors,
                    // it can be removed.
                    errors.toList.foreach(logger.error)
                    Invalid(errors)
            }
        })
    }

    def apply(
        headConfigPreinit: HeadConfig.Preinit,
        initialBlock: Block.MultiSigned.Initial
    ): ValidatedNel[String, HeadConfig] = HeadConfig(
      headConfigPreinit,
      initialBlock.blockBrief,
      initialBlock.effects.initializationTx.tx,
      initialBlock.effects.fallbackTx.tx
    )

    def apply(
        cardanoNetwork: CardanoNetwork,
        headParams: HeadParameters,
        headPeers: HeadPeers,
        initialBlock: Block.MultiSigned.Initial,
        initializationParams: InitializationParameters,
    ): ValidatedNel[String, HeadConfig] = {
        HeadConfig
            .Preinit(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams
            )
            .andThen(headConfigPreinit => HeadConfig(headConfigPreinit, initialBlock))
    }

    trait Section extends HeadConfig.Preinit.Section, InitialBlock.Section {
        def headConfig: HeadConfig

        override transparent inline def initialBlockSection: InitialBlock = InitialBlock(
          initialBlock
        )

        override def cardanoNetwork: CardanoNetwork =
            headConfigPreinit.cardanoNetwork
        override def headParams: HeadParameters = headConfigPreinit.headParams
        override def headPeers: HeadPeers = headConfigPreinit.headPeers
        override def initializationParams: InitializationParameters =
            headConfigPreinit.initializationParams
    }

    /** @param l2Params
      *   a black-box, L2-specific blake2b-256 hash of parameters that the peers must agree on
      *   before initialization.
      */
    final case class Preinit private[head] (
        override val cardanoNetwork: CardanoNetwork,
        override val headParams: HeadParameters,
        override val headPeers: HeadPeers,
        override val initializationParams: InitializationParameters,
    ) extends Preinit.Section {
        override transparent inline def headConfigPreinit: Preinit = this
    }

    object Preinit {
        // TODO This encoder should be a little bit more intelligent.
        // We want explicit indicies from vkey -> info, such as
        // - vkey -> connection address
        // - vkey -> equity
        // - vkey -> peer number
        given headConfigPreinitEncoder(using CardanoNetwork.Section): Encoder[HeadConfig.Preinit]
        with {
            override def apply(hc: HeadConfig.Preinit): Json = {
                given HeadConfig.Preinit.Section = hc
                Json.obj(
                  "cardanoNetwork" -> hc.cardanoNetwork.asJson,
                  "headParams" -> hc.headParams.asJson,
                  "headPeers" -> hc.headPeers.asJson,
                  "initialEvacuationMap" -> hc.initialEvacuationMap.asJson,
                  "initialEquityContributions" -> hc.initialEquityContributions.toSortedMap.asJson,
                  "seedUtxo" -> hc.seedUtxo.asJson,
                  "additionalFundingUtxos" -> hc.initialAdditionalFundingUtxos.asJson,
                  "initialChangeOutputs" -> hc.initialChangeOutputs.asJson
                )
            }
        }

        given headConfigPreinitDecoder(using CardanoNetwork.Section): Decoder[HeadConfig.Preinit]
        with {
            override def apply(c: HCursor): Result[Preinit] =
                for {
                    cardanoNetwork <- c.downField("cardanoNetwork").as[CardanoNetwork]
                    headPeers <- c.downField("headPeers").as[HeadPeers]
                    res <- {
                        for {
                            headParams <- c.downField("headParams").as[HeadParameters]
                            initParams <- c.as[InitializationParameters]
                            preInit <- Preinit(
                              cardanoNetwork,
                              headParams,
                              headPeers,
                              initParams
                            ).toEither.left.map(e =>
                                io.circe.DecodingFailure(
                                  s"failure constructing head config preinit: $e",
                                  c.history
                                )
                            )
                        } yield preInit
                    }
                } yield res
        }

        def apply(
            cardanoNetwork: CardanoNetwork,
            headParams: HeadParameters,
            headPeers: HeadPeers,
            initializationParams: InitializationParameters
        ): ValidatedNel[String, HeadConfig.Preinit] = {
            val headConfigPreinit = new HeadConfig.Preinit(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams
            )

            val isBalanced = Validated.cond(
              test = headConfigPreinit.isBalancedInitializationFunding,
              a = (),
              e = "Initialization funding is unbalanced"
            )

            val vKeysCoherent = Validated.cond(
              test =
                  initializationParams.initialEquityContributions.keys == NonEmptySet.fromSetUnsafe(
                    SortedSet.from(headPeers.headPeerNums.toList)
                  ),
              a = (),
              e = "initialEquityContributions and headPeers don't contain the same peer numbers"
            )

            List(
              isBalanced,
              vKeysCoherent
            ).foldLeft(Valid(()): ValidatedNel[String, Unit])((x, y) =>
                x.combine(y.leftMap(NonEmptyList.one))
            ) match {
                case Valid(()) => Valid(headConfigPreinit)
                case x @ Invalid(errors) => {
                    // We log in the constructor rather than the pattern match. If this causes spurious errors,
                    // it can be removed.
                    errors.toList.foreach(logger.error)
                    Invalid(errors)
                }
            }
        }

        trait Section
            extends CardanoNetwork.Section,
              HeadParameters.Section,
              HeadPeers.Section,
              InitializationParameters.Section {
            def headConfigPreinit: HeadConfig.Preinit

            override transparent inline def l2ParamsHash: Hash32 = headParams.l2ParamsHash

            override transparent inline def settlementConfig: SettlementConfig =
                headParams.settlementConfig
            override transparent inline def cardanoInfo: CardanoInfo = cardanoNetwork.cardanoInfo
            override transparent inline def network: Network = cardanoNetwork.network
            override transparent inline def slotConfig: SlotConfig = cardanoNetwork.slotConfig
            override transparent inline def cardanoProtocolParams: ProtocolParams =
                cardanoNetwork.cardanoProtocolParams

            override transparent inline def txTiming: TxTiming = headParams.txTiming

            override transparent inline def fallbackContingency: FallbackContingency =
                headParams.fallbackContingency

            override transparent inline def disputeResolutionConfig: DisputeResolutionConfig =
                headParams.disputeResolutionConfig

            override transparent inline def headParamsHash: Hash32 =
                headParams.headParamsHash

            override def headPeerNums: NonEmptyList[HeadPeerNumber] =
                headPeers.headPeerNums
            override transparent inline def headPeerIds: NonEmptyList[HeadPeerId] =
                headPeers.headPeerIds
            override transparent inline def headPeerVKeys: NonEmptyList[VerificationKey] =
                headPeers.headPeerVKeys
            override transparent inline def headPeerVKey(
                p: HeadPeerNumber
            ): Option[VerificationKey] =
                headPeers.headPeerVKey(p)
            override transparent inline def headPeerVKey(
                p: HeadPeerId
            ): Option[VerificationKey] =
                headPeers.headPeerVKey(p)
            override transparent inline def nHeadPeers: PositiveInt =
                headPeers.nHeadPeers
            override transparent inline def headMultisigScript: HeadMultisigScript =
                headPeers.headMultisigScript

            // TODO: this type allows non-babbage outputs
            override transparent inline def initialEvacuationMap: EvacuationMap =
                initializationParams.initialEvacuationMap
            override transparent inline def initialEquityContributions
                : NonEmptyMap[HeadPeerNumber, Coin] =
                initializationParams.initialEquityContributions
            override transparent inline def seedUtxo: Utxo =
                initializationParams.seedUtxo
            override transparent inline def headId: HeadId =
                initializationParams.headId
            override transparent inline def initialAdditionalFundingUtxos: Utxos =
                initializationParams.initialAdditionalFundingUtxos
            override transparent inline def initialChangeOutputs: List[TransactionOutput] =
                initializationParams.initialChangeOutputs
            override transparent inline def initialEquityContributed: Coin =
                initializationParams.initialEquityContributed
            override transparent inline def initialFundingValue: Value =
                initializationParams.initialFundingValue
            override transparent inline def initialL2Value: Value =
                initializationParams.initialL2Value
            override transparent inline def headTokenNames: CIP67.HeadTokenNames =
                initializationParams.headTokenNames
            override transparent inline def initialEquityContributionsHash: Hash32 =
                initializationParams.initialEquityContributionsHash

            override transparent inline def ruleBasedScriptAddresses: RuleBasedScriptAddresses =
                cardanoNetwork.ruleBasedScriptAddresses

            override transparent inline def ruleBasedTreasuryAddress: ShelleyAddress =
                ruleBasedScriptAddresses.ruleBasedTreasuryAddress

            override transparent inline def ruleBasedDisputeResolutionAddress: ShelleyAddress =
                ruleBasedScriptAddresses.ruleBasedDisputeResolutionAddress
        }
    }
}

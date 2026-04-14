package hydrozoa.config.head

import cats.data.{NonEmptyList, NonEmptyMap}
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
import io.circe.generic.semiauto.*
import io.circe.{Encoder, *}
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
                    hc <- HeadConfig(preinit, brief, initTx, fallbackTx)
                        .toRight(
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
    ): Option[HeadConfig] = {
        def isMultiSigned(tx: Transaction, headPeers: HeadPeers.Section): Boolean = {
            val witnesses = tx.witnessSetRaw.value.vkeyWitnesses.toSet

            headPeers.headPeerVKeys.forall(vKey =>
                witnesses.find(witness => witness.vkey == vKey) match {
                    case None => false
                    case Some(witness) =>
                        platform.verifyEd25519Signature(vKey, tx.id, witness.signature)
                }
            )
        }

        for {
            expectedTxSeq <- InitializationTxSeq
                .Build(headConfigPreinit)(blockBrief.endTime)
                .result
                .toOption
            // Check that tx bodies are the same
            hc <-
                if expectedTxSeq.initializationTx.tx.body == initTx.body
                    && expectedTxSeq.fallbackTx.tx.body == fallbackTx.body
                    && isMultiSigned(initTx, headConfigPreinit)
                    && isMultiSigned(fallbackTx, headConfigPreinit)
                then {

                    Some(
                      new HeadConfig(
                        headConfigPreinit,
                        Block.MultiSigned.Initial(
                          blockBrief,
                          // The "expectedTxSeq" contains the "enriched" tx types, but they are not signed,
                          // so we steal the tx signatures here here.
                          BlockEffects.MultiSigned.Initial(
                            expectedTxSeq.initializationTx.copy(tx = initTx),
                            expectedTxSeq.fallbackTx.copy(tx = fallbackTx)
                          )
                        )
                      )
                    )
                } else None
        } yield hc
    }

    // FIXME: Do a `ValidatedNel[Error, HeadConfig]` here
    def apply(
        headConfigPreinit: HeadConfig.Preinit,
        initialBlock: Block.MultiSigned.Initial
    ): Option[HeadConfig] = HeadConfig(
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
    ): Option[HeadConfig] = {
        for {
            headConfigPreinit <- HeadConfig.Preinit(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams
            )
            result <- HeadConfig(headConfigPreinit, initialBlock)
        } yield result

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
        given headConfigPreinitEncoder(using CardanoNetwork.Section): Encoder[HeadConfig.Preinit] =
            deriveEncoder[HeadConfig.Preinit]
        //            with {
        //            override def apply(hc: HeadConfig.Preinit): Json = {
        //                given HeadConfig.Preinit.Section = hc
        //
        //                val headPeersEquity: TreeMap[VerificationKey, Coin] =
        //                    hc.initializationParams.initialEquityContributions.toNel
        //                        .foldLeft(TreeMap.empty[VerificationKey, Coin]) { case (m, (peerNum, equity)) =>
        //                            m.updated(hc.headPeers.headPeerVKey(peerNum).get, equity)
        //                        }
        //
        //                Json.obj(
        //                  "cardanoNetwork" -> hc.cardanoNetwork.asJson,
        //                  "headParams" -> hc.headParams.asJson,
        //                  "initialEvacuationMap" -> hc.initialEvacuationMap.asJson,
        //                  "headPeersAndEquity" -> headPeersEquity.asJson,
        //                  "seedUtxo" -> hc.initialSeedUtxo.asJson,
        //                  "headId" -> hc.headId.asJson,
        //                  "additionalFundingUtxos" -> hc.initialAdditionalFundingUtxos.asJson,
        //                  "changeOutputs" -> hc.initialChangeOutputs.asJson
        //                )
        //            }
        //        }

        given headConfigPreinitDecoder(using CardanoNetwork.Section): Decoder[HeadConfig.Preinit] =
            deriveDecoder[HeadConfig.Preinit]
        //         with {
        //            override def apply(c: HCursor): Decoder.Result[HeadConfig.Preinit] =
        //                for {
        //                    cardanoNetwork <- c.downField("cardanoNetwork").as[CardanoNetwork]
        //                    res <- {
        //                        given CardanoNetwork.Section = cardanoNetwork
        //
        //                        for {
        //                            headParams <- c.downField("headParams").as[HeadParameters]
        //                            headPeersEquity <- c
        //                                .downField("headPeersAndEquity")
        //                                .as[Map[VerificationKey, Coin]]
        //                            headPeers <-
        //                                if headPeersEquity.nonEmpty
        //                                then Right(HeadPeers(headPeersEquity.keys.toList).get)
        //                                else
        //                                    Left(
        //                                      io.circe.DecodingFailure(
        //                                        "headPeersAndEquity must contain at least one peer",
        //                                        c.history
        //                                      )
        //                                    )
        //                            initialEquityContributions = NonEmptyMap.fromMapUnsafe(
        //                              headPeersEquity.values.zipWithIndex
        //                                  .foldLeft(TreeMap.empty[HeadPeerNumber, Coin]) {
        //                                      case (m, (coin, peerIdx)) =>
        //                                          m ++ Seq((HeadPeerNumber(peerIdx), coin))
        //                                  }
        //                            )
        //                            initialEvacuationMap <- c
        //                                .downField("initialEvacuationMap")
        //                                .as[EvacuationMap]
        //                            initialSeedUtxo <- c.downField("seedUtxo").as[Utxo]
        //                            headId <- c.downField("headId").as[HeadId]
        //                            initialAdditionalFundingUtxos <- c
        //                                .downField("additionalFundingUtxos")
        //                                .as[Utxos]
        //                            initialChangeOutputs <- c
        //                                .downField("changeOutputs")
        //                                .as[List[TransactionOutput]]
        //                            initParams = InitializationParameters(
        //                              initialEvacuationMap,
        //                              initialEquityContributions,
        //                              initialSeedUtxo,
        //                              headId,
        //                              initialAdditionalFundingUtxos,
        //                              initialChangeOutputs
        //                            )
        //                            hcpi <- HeadConfig
        //                                .Preinit(
        //                                  cardanoNetwork,
        //                                  headParams,
        //                                  headPeers,
        //                                  initParams
        //                                )
        //                                .toRight(
        //                                  io.circe.DecodingFailure(
        //                                    "Failed constructing HeadConfig.Preinit",
        //                                    c.history
        //                                  )
        //                                )
        //                        } yield hcpi
        //                    }
        //                } yield res
        //        }

        def apply(
            cardanoNetwork: CardanoNetwork,
            headParams: HeadParameters,
            headPeers: HeadPeers,
            initializationParams: InitializationParameters
        ): Option[HeadConfig.Preinit] = {
            val headConfigPreinit = new HeadConfig.Preinit(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams
            )

            val isBalancedInitializationFunding = headConfigPreinit.isBalancedInitializationFunding

            if !isBalancedInitializationFunding then {
                logger.error("Initialization funding is unbalanced.")
            }

            Option.when(isBalancedInitializationFunding)(headConfigPreinit)
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
            override transparent inline def initialSeedUtxo: Utxo =
                initializationParams.initialSeedUtxo
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

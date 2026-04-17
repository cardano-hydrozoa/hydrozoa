package hydrozoa.config.head

import cats.*
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.effect.*
import cats.syntax.all.*
import hydrozoa.config
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.ScriptReferenceUtxos.{DisputeScriptUtxo, TreasuryScriptUtxo, given_Decoder_Unresolved}
import hydrozoa.config.head.HeadConfig.Bootstrap.HeadConfigBootstrapError
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork.{Custom, cardanoNetworkDecoder}
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.peers.HeadPeers.headPeersDecoder
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.head.rulebased.scripts.RuleBasedScriptAddresses
import hydrozoa.config.node.NodePrivateConfig.nodePrivateConfigDecoder
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.lib.logging.Logging
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx, Metadata as MD, Tx}
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import io.circe.syntax.*
import io.circe.{Encoder, *}
import scala.collection.immutable.SortedSet
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.VerificationKey

/** Invariant: this _must_ be able to project down to a HeadConfig.Bootstrap
  */
final case class HeadConfig private (
    override val cardanoNetwork: CardanoNetwork,
    override val headParams: HeadParameters,
    override val headPeers: HeadPeers,
    _initialEvacuationMap: EvacuationMap,
    _initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin],
    override val scriptReferenceUtxos: ScriptReferenceUtxos,
    override val initialBlock: Block.MultiSigned.Initial,
) extends HeadConfig.Section {
    override transparent inline def headConfig: HeadConfig = this

    override def headConfigBootstrap: HeadConfig.Bootstrap = {
        val initTx = initialBlock.effects.initializationTx

        val initializationParameters: InitializationParameters = InitializationParameters(
          initialEvacuationMap = _initialEvacuationMap,
          initialEquityContributions = _initialEquityContributions,
          seedUtxo = initTx.seedUtxo,
          additionalFundingUtxos = initTx.additionalFundingUtxos,
          initialChangeOutputs = initTx.changeUtxos
        )
        new HeadConfig.Bootstrap(
          cardanoNetwork,
          headParams,
          headPeers,
          initializationParameters,
          scriptReferenceUtxos
        )
    }
}

object HeadConfig {

    def fromJson[F[_]](
        jsonStr: String,
        cardanoBackend: CardanoBackend[F]
    )(using
        monadF: Monad[F]
    ): EitherT[F, ScriptReferenceUtxos.Error | io.circe.Error, HeadConfig] = {
        for {
            unresolved <- EitherT.fromEither[F] {
                given onlyScripRefs: Decoder[ScriptReferenceUtxos.Unresolved] =
                    Decoder.instance(c =>
                        c.downField("scriptReferenceUtxos")
                            .as[ScriptReferenceUtxos.Unresolved](using given_Decoder_Unresolved)
                    )
                parser.decode[ScriptReferenceUtxos.Unresolved](jsonStr)
            }
            network <- EitherT.fromEither[F] {
                given onlyNetwork: Decoder[CardanoNetwork] = Decoder.instance(c =>
                    c.downField("cardanoNetwork")
                        .as[CardanoNetwork](using cardanoNetworkDecoder)
                )
                parser.decode[CardanoNetwork](jsonStr)
            }
            resolved <- {
                given CardanoNetwork.Section = network
                EitherT(unresolved.resolve(cardanoBackend))
            }
            headConfig <- EitherT.fromEither[F] {
                given ScriptReferenceUtxos = resolved
                parser.decode[HeadConfig](jsonStr)
            }

        } yield headConfig
    }

    given headConfigEncoder: Encoder[HeadConfig] with {
        override def apply(hc: HeadConfig): Json = {
            given HeadConfig.Section = hc
            Json.obj(
              "cardanoNetwork" -> hc.cardanoNetwork.asJson,
              "headParams" -> hc.headParams.asJson,
              "headPeers" -> hc.headPeers.asJson,
              "initialEvacuationMap" -> hc.initialEvacuationMap.asJson,
              "initialEquityContributions" -> hc._initialEquityContributions.asJson,
              "scriptReferenceUtxos" -> hc.scriptReferenceUtxos.unresolved.asJson,
              "initialBlock" -> hc.initialBlock.asJson,
              "seedUtxo" -> hc.seedUtxo.asJson,
              "additionalFundingUtxos" -> hc.additionalFundingUtxos.asJson
            )
        }
    }

    given headConfigDecoder(using resolved: ScriptReferenceUtxos): Decoder[HeadConfig] =
        Decoder.instance { c =>
            for {
                network <- c
                    .downField("cardanoNetwork")
                    .as[CardanoNetwork]
                hc <- {
                    given CardanoNetwork = network
                    for {
                        // NOTE: we can't just parse a full "Block.Multisigned.Initial", because this
                        // requires a _semantic_ parsing of the initialization/fallback transactions.
                        // We don't/can't do that until we construct the head config.

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
                        hcBootstrap <- c.as[HeadConfig.Bootstrap](using
                          HeadConfig.Bootstrap.withInitTxDecoder(initTx)
                        )

                        hc <- HeadConfig(
                          hcBootstrap,
                          brief,
                          initTx,
                          fallbackTx
                        ).toEither.left.map(e =>
                            io.circe.DecodingFailure(s"Failed to decode HeadConfig: $e", c.history)
                        )
                    } yield hc
                }
            } yield hc
        }

    private val logger = Logging.logger("HeadConfig")

    type HeadConfigError = Tx[
      InitializationTx
    ]#SignatureError | Tx[FallbackTx]#SignatureError | InitializationTxSeq.Build.Error |
        HeadConfigBootstrapError

    def apply(
        headConfigBootstrap: HeadConfig.Bootstrap,
        blockBrief: BlockBrief.Initial,
        initTxSigned: Transaction,
        fallbackTxSigned: Transaction
    ): ValidatedNel[HeadConfigError, HeadConfig] = {
        val validatedUnsignedInitTxSeq = Validated.fromEither(
          InitializationTxSeq
              .Build(headConfigBootstrap)(blockBrief.endTime)
              .result
              .left
              .map(error => NonEmptyList.one(error))
        )

        validatedUnsignedInitTxSeq.andThen(expectedTxSeq => {
            val validatedSignedInitTx
                : ValidatedNel[Tx[InitializationTx]#SignatureError, InitializationTx] =
                expectedTxSeq.initializationTx
                    .validateAndAddMultiSignatures(headConfigBootstrap.headPeers, initTxSigned)

            val validatedSignedFallbackTx: ValidatedNel[Tx[FallbackTx]#SignatureError, FallbackTx] =
                expectedTxSeq.fallbackTx
                    .validateAndAddMultiSignatures(headConfigBootstrap.headPeers, fallbackTxSigned)

            (validatedSignedInitTx, validatedSignedFallbackTx) match {
                case (Valid(_), Invalid(e))     => Invalid(e)
                case (Invalid(e), Valid(_))     => Invalid(e)
                case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2.toList)
                case (Valid(iTx), Valid(fTx)) =>
                    val hc = new HeadConfig(
                      cardanoNetwork = headConfigBootstrap.cardanoNetwork,
                      headParams = headConfigBootstrap.headParams,
                      headPeers = headConfigBootstrap.headPeers,
                      _initialEvacuationMap = headConfigBootstrap.initialEvacuationMap,
                      _initialEquityContributions = headConfigBootstrap.initialEquityContributions,
                      scriptReferenceUtxos = headConfigBootstrap.scriptReferenceUtxos,
                      Block.MultiSigned.Initial(
                        blockBrief,
                        // The "expectedTxSeq" contains the "enriched" tx types, but they are not signed,
                        // so we steal the tx signatures here.
                        BlockEffects.MultiSigned.Initial(iTx, fTx)
                      )
                    )
                    Valid(hc)
            }
        })
    }

    def apply(
        headConfigBootstrap: HeadConfig.Bootstrap,
        initialBlock: Block.MultiSigned.Initial
    ): ValidatedNel[HeadConfigError, HeadConfig] = HeadConfig(
      headConfigBootstrap,
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
        scriptReferenceUtxos: ScriptReferenceUtxos
    ): ValidatedNel[HeadConfigError, HeadConfig] = {
        HeadConfig
            .Bootstrap(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams,
              scriptReferenceUtxos
            )
            .andThen(headConfigBootstrap => HeadConfig(headConfigBootstrap, initialBlock))
    }

    trait Section extends HeadConfig.Bootstrap.Section, InitialBlock.Section {
        def headConfig: HeadConfig

        override transparent inline def initialBlockSection: InitialBlock = InitialBlock(
          initialBlock
        )

        override def scriptReferenceUtxos: ScriptReferenceUtxos =
            headConfigBootstrap.scriptReferenceUtxos
        override def cardanoNetwork: CardanoNetwork =
            headConfigBootstrap.cardanoNetwork
        override def headParams: HeadParameters = headConfigBootstrap.headParams
        override def headPeers: HeadPeers = headConfigBootstrap.headPeers
        override def initializationParams: InitializationParameters =
            headConfigBootstrap.initializationParams
    }

    /** @param l2Params
      *   a black-box, L2-specific blake2b-256 hash of parameters that the peers must agree on
      *   before initialization.
      */
    final case class Bootstrap private[head] (
        override val cardanoNetwork: CardanoNetwork,
        override val headParams: HeadParameters,
        override val headPeers: HeadPeers,
        override val initializationParams: InitializationParameters,
        override val scriptReferenceUtxos: ScriptReferenceUtxos
    ) extends Bootstrap.Section {
        override transparent inline def headConfigBootstrap: Bootstrap = this
    }

    object Bootstrap {

        /** Given JSON strings for the bootstrap config and node private config, resolve the script
          * reference utxos and return a full bootstrap config
          */
        def fromJson(
            bootstrapConfigStr: String,
            nodePrivateConfigStr: String
        ): EitherT[IO, ScriptReferenceUtxos.Error | io.circe.Error, Bootstrap] =
            for {
                network <- EitherT.fromEither[IO] {
                    given onlyNetwork: Decoder[CardanoNetwork] = Decoder.instance(c =>
                        c.downField("cardanoNetwork")
                            .as[CardanoNetwork](using cardanoNetworkDecoder)
                    )

                    parser.decode(bootstrapConfigStr)
                }
                headPeers <- EitherT.fromEither[IO] {
                    given onlyHeadPeers: Decoder[HeadPeers] = Decoder.instance(c =>
                        c.downField("headPeers")
                            .as[HeadPeers](using headPeersDecoder)
                    )

                    parser.decode(bootstrapConfigStr)
                }

                privateConfig <- EitherT.fromEither[IO] {
                    given HeadPeers = headPeers

                    given CardanoNetwork = network

                    io.circe.parser.decode(nodePrivateConfigStr)(using nodePrivateConfigDecoder)
                }

                blockfrostNetwork = network match {
                    case n: StandardCardanoNetwork => Left(n)
                    // TODO: need a blockfrost url here
                    case custom: Custom => Right((custom, ??? : CardanoBackendBlockfrost.URL))
                }

                cardanoBackend <- EitherT.liftF(
                  CardanoBackendBlockfrost(blockfrostNetwork, privateConfig.blockfrostApiKey)
                )

                bootstrapConfig <- Bootstrap.fromJson(bootstrapConfigStr, cardanoBackend)

            } yield bootstrapConfig

        /** Read the bootstrap configuration from a string, resolving the script references with the
          * provided cardano backend
          * @return
          */
        def fromJson[F[_]](
            jsonStr: String,
            cardanoBackend: CardanoBackend[F]
        )(using
            monadF: Monad[F]
        ): EitherT[F, ScriptReferenceUtxos.Error | io.circe.Error, Bootstrap] = {
            for {
                unresolved <- EitherT.fromEither[F] {
                    given onlyScripRefs: Decoder[ScriptReferenceUtxos.Unresolved] =
                        Decoder.instance(c =>
                            c.downField("scriptReferenceUtxos")
                                .as[ScriptReferenceUtxos.Unresolved](using given_Decoder_Unresolved)
                        )

                    parser.decode[ScriptReferenceUtxos.Unresolved](jsonStr)
                }
                network <- EitherT.fromEither[F] {
                    given onlyNetwork: Decoder[CardanoNetwork] = Decoder.instance(c =>
                        c.downField("cardanoNetwork")
                            .as[CardanoNetwork](using cardanoNetworkDecoder)
                    )

                    parser.decode[CardanoNetwork](jsonStr)
                }
                resolved <- {
                    given CardanoNetwork.Section = network

                    EitherT(unresolved.resolve(cardanoBackend))
                }
                headConfig <- EitherT.fromEither[F] {
                    given ScriptReferenceUtxos = resolved
                    given CardanoNetwork.Section = network

                    parser.decode[Bootstrap](jsonStr)
                }

            } yield headConfig
        }

        // TODO This encoder should be a little bit more intelligent.
        // We want explicit indicies from vkey -> info, such as
        // - vkey -> connection address
        // - vkey -> equity
        // - vkey -> peer number
        given headConfigBootstrapEncoder(using CardanoNetwork.Section): Encoder[
          HeadConfig.Bootstrap
        ] with {
            override def apply(hc: HeadConfig.Bootstrap): Json = {
                given HeadConfig.Bootstrap.Section = hc
                Json.obj(
                  "cardanoNetwork" -> hc.cardanoNetwork.asJson,
                  "headParams" -> hc.headParams.asJson,
                  "headPeers" -> hc.headPeers.asJson,
                  "initialEvacuationMap" -> hc.initialEvacuationMap.asJson,
                  "initialEquityContributions" -> hc.initialEquityContributions.toSortedMap.asJson,
                  "seedUtxo" -> hc.seedUtxo.asJson,
                  "additionalFundingUtxos" -> hc.additionalFundingUtxos.asJson,
                  "initialChangeOutputs" -> hc.initialChangeOutputs.asJson,
                  "scriptReferenceUtxos" -> hc.scriptReferenceUtxos.scriptReferenceUtxosUnresolved.asJson
                )
            }
        }

        // The utxos are resolved during parsing; we fail fast.
        given headConfigBootstrapDecoder(using
            network: CardanoNetwork.Section,
            resolved: ScriptReferenceUtxos
        ): Decoder[HeadConfig.Bootstrap] = c => {
            for {
                unresolved <- c
                    .downField("scriptReferenceUtxos")
                    .as[ScriptReferenceUtxos.Unresolved](using given_Decoder_Unresolved)
                _ <-
                    if unresolved.isValidResolution(resolved)
                    then Right(())
                    else
                        Left(
                          DecodingFailure("invalid resolution for ScriptReferenceUtxos", c.history)
                        )
                cardanoNetwork <- c
                    .downField("cardanoNetwork")
                    .as[CardanoNetwork](using cardanoNetworkDecoder)
                headPeers <- c.downField("headPeers").as[HeadPeers]
                res <- {
                    for {
                        headParams <- c.downField("headParams").as[HeadParameters]
                        initParams <- c.as[InitializationParameters]
                        bootstrap <- Bootstrap(
                          cardanoNetwork,
                          headParams,
                          headPeers,
                          initParams,
                          resolved
                        ).toEither.left.map(e =>
                            io.circe.DecodingFailure(
                              s"failure constructing head config bootstrap: $e",
                              c.history
                            )
                        )
                    } yield bootstrap
                }
            } yield res
        }

        /** A decoder without the `initialChangeOutput` field, parsing them from the raw
          * initialization tx instead
          * @param block
          * @param network
          * @param resolved
          * @return
          */
        def withInitTxDecoder(initTx: Transaction)(using
            network: CardanoNetwork.Section,
            resolved: ScriptReferenceUtxos
        ): Decoder[HeadConfig.Bootstrap] = Decoder.instance(c =>
            for {
                headParams <- c.downField("headParams").as[HeadParameters]
                headPeers <- c.downField("headPeers").as[HeadPeers]
                initialEvacuationMap <- c
                    .downField("initialEvacuationMap")
                    .as[EvacuationMap]
                initialEquityContributions <- c
                    .downField("initialEquityContributions")
                    .as[NonEmptyMap[HeadPeerNumber, Coin]]
                seedUtxo <- c.downField("seedUtxo").as[Utxo]
                srus <- for {
                    unresolved <- c
                        .downField("scriptReferenceUtxos")
                        .as[ScriptReferenceUtxos.Unresolved]
                    res <-
                        if unresolved.isValidResolution(resolved)
                        then Right(resolved)
                        else
                            Left(
                              io.circe.DecodingFailure(
                                "Failed to resolved script reference utxos",
                                c.history
                              )
                            )
                } yield res

                additionalFundingUtxos <- c.downField("additionalFundingUtxos").as[Utxos]
                mdParseResult <- MD.Initialization
                    .parse(initTx)
                    .left
                    .map(e =>
                        io.circe.DecodingFailure(
                          s"Failed decode HeadConfig.Bootstrap from the initializationTx: ${e.toString}",
                          c.history
                        )
                    )

                initialChangeOutputs = {
                    initTx.body.value.outputs
                        .map(_.value)
                        .zipWithIndex
                        .filter((_, idx) =>
                            idx != mdParseResult._2.multisigRegimeIx && idx != mdParseResult._2.multisigTreasuryIx
                        )
                        .map(_._1)
                        .toList
                }

                initParams = InitializationParameters(
                  initialEvacuationMap = initialEvacuationMap,
                  initialEquityContributions = initialEquityContributions,
                  seedUtxo = seedUtxo,
                  additionalFundingUtxos = additionalFundingUtxos,
                  initialChangeOutputs = initialChangeOutputs
                )
                bootstrapConfig <- HeadConfig
                    .Bootstrap(
                      network.cardanoNetwork,
                      headParams,
                      headPeers,
                      initParams,
                      srus
                    )
                    .toEither
                    .left
                    .map(e =>
                        io.circe.DecodingFailure(
                          "Failed to decode HeadConfig.Bootstrap from the initialization transaction: " ++ e.toString,
                          c.history
                        )
                    )
            } yield bootstrapConfig
        )

        // TODO: Make this typed better
        type HeadConfigBootstrapError = String

        def apply(
            cardanoNetwork: CardanoNetwork,
            headParams: HeadParameters,
            headPeers: HeadPeers,
            initializationParams: InitializationParameters,
            scriptReferenceUtxos: ScriptReferenceUtxos
        ): ValidatedNel[HeadConfigBootstrapError, HeadConfig.Bootstrap] = {
            val headConfigBootstrap = new HeadConfig.Bootstrap(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams,
              scriptReferenceUtxos
            )

            val isBalanced = Validated.cond(
              test = headConfigBootstrap.isBalancedInitializationFunding,
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
                case Valid(()) => Valid(headConfigBootstrap)
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
              InitializationParameters.Section,
              ScriptReferenceUtxos.Section {
            def headConfigBootstrap: HeadConfig.Bootstrap

            override transparent inline def rulebasedTreasuryScriptUtxo: TreasuryScriptUtxo = {
                scriptReferenceUtxos.rulebasedTreasuryScriptUtxo
            }

            override transparent inline def disputeResolutionScriptUtxo: DisputeScriptUtxo =
                scriptReferenceUtxos.disputeResolutionScriptUtxo

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
            override transparent inline def additionalFundingUtxos: Utxos =
                initializationParams.additionalFundingUtxos
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

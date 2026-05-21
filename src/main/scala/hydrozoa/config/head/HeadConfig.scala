package hydrozoa.config.head

import cats.*
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.effect.*
import cats.syntax.all.*
import hydrozoa.config
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.ScriptReferenceUtxos.given_Decoder_Unresolved
import hydrozoa.config.head.HeadConfig.Bootstrap.HeadConfigBootstrapError
import hydrozoa.config.head.coil.CoilPeer
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.network.CardanoNetwork.{Custom, cardanoNetworkDecoder}
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.peers.HeadPeers.headPeersDecoder
import hydrozoa.config.node.NodePrivateConfig.given
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import io.circe.syntax.*
import io.circe.{Encoder, *}
import scala.collection.immutable.SortedSet
import scalus.cardano.ledger.*

/** Invariant: this _must_ be able to project down to a HeadConfig.Bootstrap
  */
final case class HeadConfig private (
    override val cardanoNetwork: CardanoNetwork,
    override val headParameters: HeadParameters,
    override val headPeers: HeadPeers,
    override val coilPeers: List[CoilPeer],
    _initialEvacuationMap: EvacuationMap,
    _initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin],
    override val scriptReferenceUtxos: ScriptReferenceUtxos,
    override val initialBlockSection: InitialBlock,
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
          headParameters,
          headPeers,
          coilPeers,
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
              "headParams" -> hc.headParameters.asJson,
              "headPeers" -> hc.headPeers.asJson,
              "coilPeers" -> hc.coilPeers.asJson,
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
                        // NOTE: we can't just parse a full `Block.Unsigned.Initial`, because the
                        // initialization/fallback transaction wrappers need _semantic_ parsing
                        // (which depends on the head config we're constructing).
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

    type HeadConfigError = InitializationTxSeq.Build.Error | HeadConfigBootstrapError

    /** Construct a HeadConfig from its bootstrap context and the unsigned initial-block payload.
      *
      * The init+fallback txs are passed through unsigned — slow consensus's stack-0 hard-ack flow
      * signs them at startup.
      */
    def apply(
        headConfigBootstrap: HeadConfig.Bootstrap,
        initialBlock: InitialBlock
    ): ValidatedNel[HeadConfigError, HeadConfig] =
        Validated.valid(
          new HeadConfig(
            cardanoNetwork = headConfigBootstrap.cardanoNetwork,
            headParameters = headConfigBootstrap.headParameters,
            headPeers = headConfigBootstrap.headPeers,
            coilPeers = headConfigBootstrap.coilPeers,
            _initialEvacuationMap = headConfigBootstrap.initialEvacuationMap,
            _initialEquityContributions = headConfigBootstrap.initialEquityContributions,
            scriptReferenceUtxos = headConfigBootstrap.scriptReferenceUtxos,
            initialBlock
          )
        )

    /** Decoder-side adapter: builds the unsigned init+fallback wrappers from the raw `Transaction`
      * bytes parsed out of JSON, then delegates to the [[InitialBlock]]-shaped apply. The raw
      * `fallbackTx` is discarded (the fallback is deterministically derivable from the bootstrap +
      * brief via [[InitializationTxSeq.Build]]); only `initTx` round-trips faithfully.
      */
    def apply(
        headConfigBootstrap: HeadConfig.Bootstrap,
        blockBrief: BlockBrief.Initial,
        initTx: Transaction,
        fallbackTx: Transaction
    ): ValidatedNel[HeadConfigError, HeadConfig] = {
        val _ = fallbackTx // currently advisory; consistency-checked via Build below
        Validated
            .fromEither(
              InitializationTxSeq
                  .Build(headConfigBootstrap)(blockBrief.endTime)
                  .result
                  .left
                  .map(error => NonEmptyList.one(error))
            )
            .andThen { expected =>
                val ib = InitialBlock(
                  Block.Unsigned.Initial(
                    blockBrief,
                    BlockEffects.Unsigned.Initial(
                      initializationTx = expected.initializationTx,
                      fallbackTx = expected.fallbackTx
                    )
                  )
                )
                HeadConfig(headConfigBootstrap, ib)
            }
    }

    def apply(
        cardanoNetwork: CardanoNetwork,
        headParams: HeadParameters,
        headPeers: HeadPeers,
        coilPeers: List[CoilPeer],
        initialBlock: InitialBlock,
        initializationParams: InitializationParameters,
        scriptReferenceUtxos: ScriptReferenceUtxos
    ): ValidatedNel[HeadConfigError, HeadConfig] = {
        HeadConfig
            .Bootstrap(
              cardanoNetwork,
              headParams,
              headPeers,
              coilPeers,
              initializationParams,
              scriptReferenceUtxos
            )
            .andThen(headConfigBootstrap => HeadConfig(headConfigBootstrap, initialBlock))
    }

    trait Section extends HeadConfig.Bootstrap.Section, InitialBlock.Section {
        def headConfig: HeadConfig

        override def headConfigBootstrap: Bootstrap = headConfig.headConfigBootstrap
        def initialBlockSection: InitialBlock = headConfig.initialBlockSection
    }

    /** @param coilPeers
      *   A mapping from the coil peer verification key to their head peer number
      * @param l2Params
      *   a black-box, L2-specific blake2b-256 hash of parameters that the peers must agree on
      *   before initialization.
      */
    final case class Bootstrap private[head] (
        override val cardanoNetwork: CardanoNetwork,
        override val headParameters: HeadParameters,
        override val headPeers: HeadPeers,
        override val coilPeers: List[CoilPeer],
        override val initializationParameters: InitializationParameters,
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
                  "headParams" -> hc.headParameters.asJson,
                  "headPeers" -> hc.headPeers.asJson,
                  "coilPeers" -> hc.coilPeers.asJson,
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
                headParams <- c.downField("headParams").as[HeadParameters]
                coilPeers <- c
                    .downField("coilPeers")
                    .as[List[CoilPeer]]
                _ <-
                    if coilPeers.length < headParams.coilQuorum
                    then
                        Left(
                          io.circe.DecodingFailure(
                            s"Error decoding HeadConfig.Bootstrap: the number of coil peers ${coilPeers.length}" +
                                s" is less than the coil quorum ${headParams.coilQuorum}",
                            c.history
                          )
                        )
                    else Right(())

                res <- {
                    for {
                        initParams <- c.as[InitializationParameters]
                        bootstrap <- Bootstrap(
                          cardanoNetwork,
                          headParams,
                          headPeers,
                          coilPeers,
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
                coilPeers <- c.downField("coilPeers").as[List[CoilPeer]]
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
                      coilPeers,
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
            coilPeers: List[CoilPeer],
            initializationParams: InitializationParameters,
            scriptReferenceUtxos: ScriptReferenceUtxos
        ): ValidatedNel[HeadConfigBootstrapError, HeadConfig.Bootstrap] = {
            val headConfigBootstrap = new HeadConfig.Bootstrap(
              cardanoNetwork,
              headParams,
              headPeers,
              coilPeers,
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
            def cardanoNetwork: CardanoNetwork = headConfigBootstrap.cardanoNetwork
            def headParameters: HeadParameters = headConfigBootstrap.headParameters
            def headPeers: HeadPeers = headConfigBootstrap.headPeers
            def coilPeers: List[CoilPeer] = headConfigBootstrap.coilPeers
            def initializationParameters: InitializationParameters =
                headConfigBootstrap.initializationParameters
            def scriptReferenceUtxos: ScriptReferenceUtxos =
                headConfigBootstrap.scriptReferenceUtxos

        }
    }
}

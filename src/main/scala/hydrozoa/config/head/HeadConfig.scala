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
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.coil.CoilPeers.coilPeersDecoder
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
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import io.circe.syntax.*
import io.circe.{Encoder, *}
import scala.collection.immutable.SortedSet
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.VerificationKey

/** Invariant: this _must_ be able to project down to a HeadConfig.Bootstrap
  */
final case class HeadConfig private (
    override val cardanoNetwork: CardanoNetwork,
    override val headParameters: HeadParameters,
    override val headPeers: HeadPeers,
    override val coilPeers: CoilPeers,
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
                        // TODO: parse not re-create, see https://linear.app/gummiworm-labs/issue/GUM-129/enrich-init-tx-metadata-implement-init-tx-parsing
                        brief <- c
                            .downField("initialBlock")
                            .downField("blockBrief")
                            .as[BlockBrief.Initial]
                        initTx <- c
                            .downField("initialBlock")
                            .downField("effects")
                            .downField("initializationTx")
                            .as[Transaction]
                        hcBootstrap <- c.as[HeadConfig.Bootstrap](using
                          HeadConfig.Bootstrap.withInitTxDecoder(initTx)
                        )

                        initTxSeq <- InitializationTxSeq
                            .Build(hcBootstrap)(brief.endTime)
                            .result
                            .left
                            .map(e =>
                                io.circe.DecodingFailure(
                                  s"Failed to derive InitializationTxSeq: $e",
                                  c.history
                                )
                            )
                        ib = InitialBlock(
                          Block.Unsigned.Initial(
                            brief,
                            BlockEffects.Unsigned.Initial(
                              initTxSeq.initializationTx,
                              initTxSeq.fallbackTx
                            )
                          )
                        )
                        hc <- HeadConfig(hcBootstrap, ib).toEither.left.map(e =>
                            io.circe.DecodingFailure(s"Failed to decode HeadConfig: $e", c.history)
                        )
                    } yield hc
                }
            } yield hc
        }

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

    def apply(
        cardanoNetwork: CardanoNetwork,
        headParams: HeadParameters,
        headPeers: HeadPeers,
        coilPeers: CoilPeers,
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
      *   The coil peers keyed by explicit [[CoilPeerNumber]] (verification key + hub head peer).
      * @param l2Params
      *   a black-box, L2-specific blake2b-256 hash of parameters that the peers must agree on
      *   before initialization.
      */
    final case class Bootstrap private[head] (
        override val cardanoNetwork: CardanoNetwork,
        override val headParameters: HeadParameters,
        override val headPeers: HeadPeers,
        override val coilPeers: CoilPeers,
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
                coilPeers <- EitherT.fromEither[IO] {
                    given onlyCoilPeers: Decoder[CoilPeers] =
                        Decoder.instance(c =>
                            c.downField("coilPeers").as[CoilPeers](using coilPeersDecoder)
                        )

                    parser.decode(bootstrapConfigStr)
                }

                privateConfig <- EitherT.fromEither[IO] {
                    given HeadPeers = headPeers

                    given List[VerificationKey] = coilPeers.verificationKeys

                    given CardanoNetwork = network

                    io.circe.parser.decode(nodePrivateConfigStr)(using nodePrivateConfigDecoder)
                }

                blockfrostNetwork = network match {
                    case n: StandardCardanoNetwork => Left(n)
                    // TODO: need a blockfrost url here
                    case custom: Custom => Right((custom, ??? : CardanoBackendBlockfrost.URL))
                }

                cardanoBackend <- EitherT.liftF(
                  CardanoBackendBlockfrost(
                    blockfrostNetwork,
                    privateConfig.blockfrostApiKey,
                    tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
                  )
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
        given headConfigBootstrapEncoder(using
            @scala.annotation.unused section: CardanoNetwork.Section
        ): Encoder[
          HeadConfig.Bootstrap
        ] with {
            override def apply(hc: HeadConfig.Bootstrap): Json = {
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
                    .as[CoilPeers]
                _ <-
                    if coilPeers.size < headParams.coilQuorum
                    then
                        Left(
                          io.circe.DecodingFailure(
                            s"Error decoding HeadConfig.Bootstrap: the number of coil peers ${coilPeers.size}" +
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
                coilPeers <- c.downField("coilPeers").as[CoilPeers]
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
            coilPeers: CoilPeers,
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
                case Valid(())       => Valid(headConfigBootstrap)
                case Invalid(errors) => Invalid(errors)
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
            def coilPeers: CoilPeers = headConfigBootstrap.coilPeers

            /** Coil peer verification keys in [[CoilPeerNumber]] order — the explicit number keyed
              * in the config, NOT a derived vkey ordering. A key's position in this list equals its
              * coil peer number, shared by the threshold native script's coil branch and by
              * hard-ack signer resolution.
              */
            final def coilPeerVKeys: List[VerificationKey] = coilPeers.verificationKeys

            /** Resolve a coil peer's verification key from its [[CoilPeerNumber]]. */
            final def coilPeerVKey(p: CoilPeerNumber): Option[VerificationKey] =
                coilPeers.verificationKey(p)

            /** The hub head peer for the given coil peer — the single head peer the coil peer links
              * to. `None` if the coil peer number is out of range.
              */
            final def coilPeerHub(p: CoilPeerNumber): Option[HeadPeerNumber] =
                coilPeers.hubHeadPeerNumber(p)

            /** The coil peers hubbed by the given head peer, by [[CoilPeerNumber]]. A hub head peer
              * spawns one peer liaison toward each of these coil peers.
              */
            final def hubbedCoilPeerNums(headNum: HeadPeerNumber): List[CoilPeerNumber] =
                coilPeers.hubbedBy(headNum)

            /** Every hub head peer — those that hub ≥ 1 coil peer (one `HubHardAck` journal each; a
              * coil peer reads them all to reconstruct the coil quorum on recovery). Empty with no
              * coil peers.
              */
            final def hubHeadPeerNumbers: List[HeadPeerNumber] =
                coilPeers.hubHeadPeerNumbers

            /** The head multisig native script including the coil threshold branch — all head peers
              * plus `MOf(coilQuorum, coilPeerVKeys)`. Overrides the head-only derivation on
              * [[HeadPeers.Section]] so every `headMultisigScript` / `headMultisigAddress` call
              * (and thus the minting policy, treasury / regime spend, and head address) resolves to
              * the threshold script. With no coil peers it is byte-identical to the head-only
              * script.
              */
            override def headMultisigScript: HeadMultisigScript =
                HeadMultisigScript(this, coilPeerVKeys, coilQuorum)

            /** The single-quantity treasury beacon token, under this head's multisig policy with
              * the CIP-67 treasury asset name derived from the seed utxo. Tests build treasury utxo
              * values as `evacMap.totalValue + treasuryToken`.
              */
            final def treasuryToken: scalus.cardano.ledger.Value =
                scalus.cardano.ledger.Value.asset(
                  headMultisigScript.policyId,
                  headTokenNames.treasuryTokenName,
                  1
                )
            def initializationParameters: InitializationParameters =
                headConfigBootstrap.initializationParameters
            def scriptReferenceUtxos: ScriptReferenceUtxos =
                headConfigBootstrap.scriptReferenceUtxos

        }
    }
}

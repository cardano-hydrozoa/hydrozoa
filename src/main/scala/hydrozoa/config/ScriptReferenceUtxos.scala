package hydrozoa.config

import cats.*
import cats.data.*
import cats.syntax.all.*
import hydrozoa.config
import hydrozoa.config.ScriptReferenceUtxos.Error.UnresolvableScriptUtxo
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.rulebased.ledger.l1.script.plutus.SetupLadder
import io.circe.*
import io.circe.generic.semiauto.*
import scalus.cardano.ledger.{DatumOption, TransactionInput, Utxo}
import scalus.cardano.txbuilder.TransactionBuilderStep.ReferenceOutput

final case class ScriptReferenceUtxos(
    override val rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo,
    override val disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo,
    override val setupLadderUtxos: ScriptReferenceUtxos.SetupLadderUtxos
) extends ScriptReferenceUtxos.Section {
    override val scriptReferenceUtxos: ScriptReferenceUtxos = this
    def toList: List[Utxo] =
        List(rulebasedTreasuryScriptUtxo.utxo, disputeResolutionScriptUtxo.utxo)
            ++ setupLadderUtxos.utxos

    def unresolved: ScriptReferenceUtxos.Unresolved =
        ScriptReferenceUtxos.Unresolved(
          rulebasedTreasuryScriptInput,
          disputeResolutionScriptInput,
          setupLadderInputs
        )
}

object ScriptReferenceUtxos {
    case class Unresolved(
        override val rulebasedTreasuryScriptInput: TransactionInput,
        override val disputeResolutionScriptInput: TransactionInput,
        override val setupLadderInputs: List[TransactionInput]
    ) extends Unresolved.Section {
        override val scriptReferenceUtxosUnresolved: Unresolved = this

        def resolve[F[_]](cardanoBackend: CardanoBackend[F])(using
            network: CardanoNetwork.Section,
            monadF: Monad[F]
        ): F[Either[ScriptReferenceUtxos.Error, ScriptReferenceUtxos]] = {

            // resolve helper
            def r(ti: TransactionInput): EitherT[F, ScriptReferenceUtxos.Error, Utxo] =
                for {
                    optionUtxo <- EitherT(cardanoBackend.resolve(ti))
                        .leftMap(ScriptReferenceUtxos.Error.CardanoBackendError(_))
                    utxo <- optionUtxo match {
                        case None       => EitherT.left(monadF.pure(UnresolvableScriptUtxo(ti)))
                        case Some(utxo) => EitherT.pure(utxo)
                    }
                } yield utxo

            for {
                treasury <- r(rulebasedTreasuryScriptInput)
                treasuryUtxo <- EitherT.fromEither(TreasuryScriptUtxo(network, treasury))
                dispute <- r(disputeResolutionScriptInput)
                disputeUtxo <- EitherT.fromEither(DisputeScriptUtxo(network, dispute))
                ladder <- setupLadderInputs.traverse(r)
                ladderUtxos <- EitherT.fromEither(SetupLadderUtxos(network, ladder))
            } yield ScriptReferenceUtxos(treasuryUtxo, disputeUtxo, ladderUtxos)
        }.value

        def isValidResolution(scriptReferenceUtxos: ScriptReferenceUtxos): Boolean =
            scriptReferenceUtxos.unresolved == this
    }

    object Unresolved {
        trait Section {
            def scriptReferenceUtxosUnresolved: Unresolved
            def rulebasedTreasuryScriptInput: TransactionInput
            def disputeResolutionScriptInput: TransactionInput
            def setupLadderInputs: List[TransactionInput]
        }
    }

    trait Section extends Unresolved.Section {
        def scriptReferenceUtxos: ScriptReferenceUtxos

        def rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo =
            scriptReferenceUtxos.rulebasedTreasuryScriptUtxo
        def disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo =
            scriptReferenceUtxos.disputeResolutionScriptUtxo
        def setupLadderUtxos: ScriptReferenceUtxos.SetupLadderUtxos =
            scriptReferenceUtxos.setupLadderUtxos

        final def referenceTreasury: ReferenceOutput = ReferenceOutput(
          rulebasedTreasuryScriptUtxo.utxo
        )
        final def referenceDispute: ReferenceOutput = ReferenceOutput(
          disputeResolutionScriptUtxo.utxo
        )

        /** The setup-ladder utxo for the smallest rung covering `k` evacuations. The caller needs
          * the utxo (not just a ReferenceOutput) to compute the `setupRefInputIdx` redeemer field.
          */
        final def setupRungUtxo(
            k: Int
        ): Either[SetupLadder.UncoveredEvacuationCount, Utxo] =
            SetupLadder.rungForEvacuations(k).map(setupLadderUtxos.utxos(_))

        /** Rung 0's outRef — the single anchor the regime datum records to authenticate the ladder
          * on-chain (rungs are outputs 0-6 of its transaction).
          */
        final def setupLadderAnchor: TransactionInput = setupLadderInputs.head

        override transparent inline def scriptReferenceUtxosUnresolved: Unresolved =
            Unresolved(
              rulebasedTreasuryScriptInput,
              disputeResolutionScriptInput,
              setupLadderInputs
            )

        override transparent inline def rulebasedTreasuryScriptInput: TransactionInput =
            scriptReferenceUtxos.rulebasedTreasuryScriptUtxo.utxo.input

        override transparent inline def disputeResolutionScriptInput: TransactionInput =
            scriptReferenceUtxos.disputeResolutionScriptUtxo.utxo.input

        override transparent inline def setupLadderInputs: List[TransactionInput] =
            scriptReferenceUtxos.setupLadderUtxos.utxos.map(_.input)
    }

    enum Error extends Throwable:
        case InvalidTreasuryScriptUtxo
        case InvalidDisputeScriptUtxo
        case InvalidSetupLadderUtxo(detail: String)
        case UnresolvableScriptUtxo(ti: TransactionInput)
        case CardanoBackendError(e: CardanoBackend.Error)

        override def toString: String = this match
            case InvalidTreasuryScriptUtxo      => "InvalidTreasuryScriptUtxo"
            case InvalidDisputeScriptUtxo       => "InvalidDisputeScriptUtxo"
            case InvalidSetupLadderUtxo(detail) => s"InvalidSetupLadderUtxo($detail)"
            case UnresolvableScriptUtxo(ti)     => s"UnresolvableScriptUtxo($ti)"
            case CardanoBackendError(e)         => s"CardanoBackendError: $e"

        override def getMessage: String = this match
            case InvalidTreasuryScriptUtxo =>
                "The provided UTXO is not a valid treasury script reference UTXO"
            case InvalidDisputeScriptUtxo =>
                "The provided UTXO is not a valid dispute resolution script reference UTXO"
            case InvalidSetupLadderUtxo(detail) =>
                s"The provided UTXOs are not a valid G2 setup ladder: $detail"
            case UnresolvableScriptUtxo(ti) =>
                s"ScriptRefUtxo with TransactionInput $ti does not currently exist according to " +
                    "the CardanoBackend. These utxos should be deployed prior to running a node. If they " +
                    "were previously deployed, please ensure they have not been spent. If they were deployed" +
                    "very recently, it may take some time for the transaction to appear onchain."
            case CardanoBackendError(e) =>
                s"Cardano backend error encountered when resolving the reference utxos: $e"

    case class TreasuryScriptUtxo private (utxo: Utxo)

    object TreasuryScriptUtxo {
        // TODO: Once we have a version script setup, we need to adjust this apply method
        def apply(
            network: CardanoNetwork.Section,
            utxo: Utxo
        ): Either[ScriptReferenceUtxos.Error, TreasuryScriptUtxo] =
            for {
                actualNetwork <- utxo.output.address.getNetwork
                    .toRight(ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo)
                _ <- Either.cond(
                  actualNetwork == network.network,
                  (),
                  ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo
                )

                scriptRef <- utxo.output.scriptRef.toRight(
                  ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo
                )

                actualHash = scriptRef.script.scriptHash
                _ <- Either.cond(
                  actualHash == hydrozoa.config.HydrozoaBlueprint.treasuryScriptHash,
                  (),
                  ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo
                )
            } yield TreasuryScriptUtxo(utxo)
    }

    case class DisputeScriptUtxo private (utxo: Utxo)

    object DisputeScriptUtxo {
        // TODO: Once we have a version script setup, we need to adjust this apply method
        def apply(
            network: CardanoNetwork.Section,
            utxo: Utxo
        ): Either[ScriptReferenceUtxos.Error, DisputeScriptUtxo] =
            for {
                actualNetwork <- utxo.output.address.getNetwork
                    .toRight(ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo)
                _ <- Either.cond(
                  actualNetwork == network.network,
                  (),
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo
                )

                scriptRef <- utxo.output.scriptRef.toRight(
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo
                )

                actualHash = scriptRef.script.scriptHash
                _ <- Either.cond(
                  actualHash == hydrozoa.config.HydrozoaBlueprint.disputeScriptHash,
                  (),
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo
                )
            } yield DisputeScriptUtxo(utxo)
    }

    case class SetupLadderUtxos private (utxos: List[Utxo])

    object SetupLadderUtxos {

        /** Validates that `utxos` are the G2 setup ladder in rung order: one utxo per rung, each on
          * the right network with an inline datum equal to the locally computed rung datum (this is
          * where the ladder's authenticity is established offchain).
          */
        def apply(
            network: CardanoNetwork.Section,
            utxos: List[Utxo]
        ): Either[ScriptReferenceUtxos.Error, SetupLadderUtxos] =
            for {
                _ <- Either.cond(
                  utxos.size == SetupLadder.rungCount,
                  (),
                  ScriptReferenceUtxos.Error.InvalidSetupLadderUtxo(
                    s"expected ${SetupLadder.rungCount} rungs, got ${utxos.size}"
                  )
                )
                // The on-chain authentication anchors on a single deployment tx: the regime
                // datum records rung 0's outRef and validators accept outputs 0-6 of its tx.
                _ <- Either.cond(
                  utxos.map(_.input) == utxos.headOption.toList.flatMap(head =>
                      List.tabulate(SetupLadder.rungCount)(i =>
                          TransactionInput(head.input.transactionId, i)
                      )
                  ),
                  (),
                  ScriptReferenceUtxos.Error.InvalidSetupLadderUtxo(
                    s"rungs must be outputs 0-${SetupLadder.rungCount - 1} of a single " +
                        "deployment transaction"
                  )
                )
                _ <- utxos.zipWithIndex.traverse_ { (utxo, i) =>
                    for {
                        actualNetwork <- utxo.output.address.getNetwork.toRight(
                          ScriptReferenceUtxos.Error.InvalidSetupLadderUtxo(
                            s"rung $i has no network in its address"
                          )
                        )
                        _ <- Either.cond(
                          actualNetwork == network.network,
                          (),
                          ScriptReferenceUtxos.Error.InvalidSetupLadderUtxo(
                            s"rung $i is on network $actualNetwork"
                          )
                        )
                        datum <- utxo.output.datumOption match {
                            case Some(DatumOption.Inline(d)) => Right(d)
                            case _ =>
                                Left(
                                  ScriptReferenceUtxos.Error.InvalidSetupLadderUtxo(
                                    s"rung $i has no inline datum"
                                  )
                                )
                        }
                        _ <- Either.cond(
                          datum == SetupLadder.rungDatum(i),
                          (),
                          ScriptReferenceUtxos.Error.InvalidSetupLadderUtxo(
                            s"rung $i datum does not match the local trusted setup"
                          )
                        )
                    } yield ()
                }
            } yield SetupLadderUtxos(utxos)
    }

    given Encoder[ScriptReferenceUtxos] = deriveEncoder[ScriptReferenceUtxos]

    given scriptReferenceUtxos(using
        network: CardanoNetwork.Section
    ): Decoder[ScriptReferenceUtxos] = deriveDecoder[ScriptReferenceUtxos]

    given Encoder[TreasuryScriptUtxo] = transactionInputAlternateEncoder.contramap(_.utxo.input)

    given treasuryReferenceScriptUtxoDecoder(using
        network: CardanoNetwork.Section,
    ): Decoder[TreasuryScriptUtxo] =
        utxoDecoder.emap(utxo =>
            TreasuryScriptUtxo(network, utxo).left.map(e =>
                "Failed to construct rule-based treasury reference script utxo." +
                    s"Failure: $e"
            )
        )

    given Encoder[DisputeScriptUtxo] = transactionInputAlternateEncoder.contramap(_.utxo.input)

    given Encoder[SetupLadderUtxos] =
        Encoder.encodeList(using transactionInputAlternateEncoder).contramap(_.utxos.map(_.input))

    given setupLadderUtxosDecoder(using
        network: CardanoNetwork.Section
    ): Decoder[SetupLadderUtxos] =
        Decoder
            .decodeList(using utxoDecoder)
            .emap(utxos =>
                SetupLadderUtxos(network, utxos).left
                    .map(e => s"Failed to construct the G2 setup ladder utxos. Failure: $e")
            )

    given disputeScriptUtxoDecoder(using
        network: CardanoNetwork.Section
    ): Decoder[DisputeScriptUtxo] =
        utxoDecoder.emap(utxo =>
            DisputeScriptUtxo(network, utxo).left.map(e =>
                "Failed to construct dispute script utxo." +
                    s"Failure: $e"
            )
        )

    given Encoder[ScriptReferenceUtxos.Unresolved] = deriveEncoder[ScriptReferenceUtxos.Unresolved]
    given Decoder[ScriptReferenceUtxos.Unresolved] = deriveDecoder[ScriptReferenceUtxos.Unresolved]
}

package hydrozoa.config

import cats.*
import cats.data.*
import cats.syntax.all.*
import hydrozoa.config
import hydrozoa.config.ScriptReferenceUtxos.Error.UnresolvableScriptUtxo
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.multisig.backend.cardano.CardanoBackend
import io.circe.*
import io.circe.generic.semiauto.*
import scalus.cardano.ledger.{TransactionInput, Utxo}
import scalus.cardano.txbuilder.TransactionBuilderStep.ReferenceOutput

final case class ScriptReferenceUtxos(
    override val rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo,
    override val disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo
) extends ScriptReferenceUtxos.Section {
    override val scriptReferenceUtxos: ScriptReferenceUtxos = this
    def toList: List[Utxo] =
        List(rulebasedTreasuryScriptUtxo.utxo, disputeResolutionScriptUtxo.utxo)

    def unresolved: ScriptReferenceUtxos.Unresolved =
        ScriptReferenceUtxos.Unresolved(rulebasedTreasuryScriptInput, disputeResolutionScriptInput)
}

object ScriptReferenceUtxos {
    case class Unresolved(
        override val rulebasedTreasuryScriptInput: TransactionInput,
        override val disputeResolutionScriptInput: TransactionInput
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
            } yield ScriptReferenceUtxos(treasuryUtxo, disputeUtxo)
        }.value

        def isValidResolution(scriptReferenceUtxos: ScriptReferenceUtxos): Boolean =
            scriptReferenceUtxos.unresolved == this
    }

    object Unresolved {
        trait Section {
            def scriptReferenceUtxosUnresolved: Unresolved
            def rulebasedTreasuryScriptInput: TransactionInput
            def disputeResolutionScriptInput: TransactionInput
        }
    }

    trait Section extends Unresolved.Section {
        def scriptReferenceUtxos: ScriptReferenceUtxos

        def rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo
        def disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo

        final def referenceTreasury: ReferenceOutput = ReferenceOutput(
          rulebasedTreasuryScriptUtxo.utxo
        )
        final def referenceDispute: ReferenceOutput = ReferenceOutput(
          disputeResolutionScriptUtxo.utxo
        )

        override transparent inline def scriptReferenceUtxosUnresolved: Unresolved =
            Unresolved(
              rulebasedTreasuryScriptInput,
              disputeResolutionScriptInput
            )

        override transparent inline def rulebasedTreasuryScriptInput: TransactionInput =
            scriptReferenceUtxos.rulebasedTreasuryScriptUtxo.utxo.input

        override transparent inline def disputeResolutionScriptInput: TransactionInput =
            scriptReferenceUtxos.disputeResolutionScriptUtxo.utxo.input
    }

    enum Error extends Throwable:
        case InvalidTreasuryScriptUtxo
        case InvalidDisputeScriptUtxo
        case UnresolvableScriptUtxo(ti: TransactionInput)
        case CardanoBackendError(e: CardanoBackend.Error)

        override def toString: String = this match
            case InvalidTreasuryScriptUtxo => "InvalidTreasuryScriptUtxo"
            case InvalidDisputeScriptUtxo  => "InvalidDisputeScriptUtxo"
            case CardanoBackendError(e)    => s"CardanoBackendError: $e"

        override def getMessage: String = this match
            case InvalidTreasuryScriptUtxo =>
                "The provided UTXO is not a valid treasury script reference UTXO"
            case InvalidDisputeScriptUtxo =>
                "The provided UTXO is not a valid dispute resolution script reference UTXO"
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

    given Encoder[ScriptReferenceUtxos] = deriveEncoder[ScriptReferenceUtxos]

    given scriptReferenceUtxos(using
        network: CardanoNetwork.Section
    ): Decoder[ScriptReferenceUtxos] = deriveDecoder[ScriptReferenceUtxos]

    // TODO This must be updated when we start getting script versions/hashses from the config
    given Encoder[TreasuryScriptUtxo] = utxoEncoder.contramap(_.utxo)

    given treasuryReferenceScriptUtxoDecoder(using
        network: CardanoNetwork.Section
    ): Decoder[TreasuryScriptUtxo] =
        utxoDecoder.emap(utxo =>
            TreasuryScriptUtxo(network, utxo).left.map(e =>
                "Failed to construct rule-based treasury reference script utxo." +
                    s"Failure: $e"
            )
        )

    given Encoder[DisputeScriptUtxo] = utxoEncoder.contramap(_.utxo)

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

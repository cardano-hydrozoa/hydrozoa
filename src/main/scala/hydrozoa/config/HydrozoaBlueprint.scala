package hydrozoa.config

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.{Script, ScriptHash}
import scalus.uplc.builtin.ByteString

/** Single source of truth for Hydrozoa script hashes and addresses, loaded from the CIP-57
  * blueprint.
  *
  * This object loads the compiled script hashes from the blueprint JSON file at compile time,
  * ensuring that runtime code and tests use the exact same script hashes without needing to compile
  * the Plutus scripts.
  */
object HydrozoaBlueprint {

    enum Error extends Throwable:
        case BlueprintLoadError(message: String, cause: Option[Throwable] = None)

        override def toString: String = this match
            case BlueprintLoadError(message, cause) =>
                cause match
                    case Some(t) => s"BlueprintLoadError: $message (caused by: ${t.getMessage})"
                    case None    => s"BlueprintLoadError: $message"

        override def getMessage: String = this match
            case BlueprintLoadError(message, _) => message

    /** Path to the blueprint file in the resources directory */
    val blueprintResourcePath: String = "/plutus.json"

    /** Path to the blueprint file in the source tree */
    val blueprintFilePath: String = "src/main/resources/plutus.json"

    /** Loads the blueprint from the classpath.
      *
      * This uses a lazy val to load once and cache the result.
      */
    private lazy val loadBlueprint: Either[Error, Blueprint] = {
        {
            val result = (for {
                stream <- Resource.fromAutoCloseable(IO {
                    val s = getClass.getResourceAsStream(blueprintResourcePath)
                    if s == null then {
                        throw new IllegalStateException(
                          s"Blueprint resource not found: $blueprintResourcePath. " +
                              "Make sure to run 'nix develop --command sbtn compile' first."
                        )
                    }
                    s
                })
                source <- Resource.fromAutoCloseable(IO(scala.io.Source.fromInputStream(stream)))
            } yield {
                val json = source.mkString
                Blueprint.fromJson(json)
            }).use(IO.pure).unsafeRunSync()

            Try(result)
        } match {
            case Success(blueprint) => Right(blueprint)
            case Failure(exception) =>
                Left(
                  Error.BlueprintLoadError(
                    s"Failed to load blueprint: ${exception.getMessage}",
                    Some(exception)
                  )
                )
        }
    }

    /** Gets the script hash for the validator with the given blueprint title. */
    private def getScriptHash(title: String): Either[Error, ScriptHash] = {
        for {
            blueprint <- loadBlueprint
            validator <- blueprint.validators
                .find(_.title == title)
                .toRight(Error.BlueprintLoadError(s"$title not found in blueprint", None))
            hashHex <- validator.hash.toRight(
              Error.BlueprintLoadError(s"$title hash not present in blueprint", None)
            )
        } yield ScriptHash.fromHex(hashHex)
    }

    /** Gets the compiled PlutusV3 script for the validator with the given blueprint title. */
    private def getScript(title: String): Either[Error, Script] = {
        for {
            blueprint <- loadBlueprint
            validator <- blueprint.validators
                .find(_.title == title)
                .toRight(Error.BlueprintLoadError(s"$title not found in blueprint", None))
            compiledCodeHex <- validator.compiledCode.toRight(
              Error.BlueprintLoadError(s"$title compiledCode not present in blueprint", None)
            )
            scriptBytes <- Try(ByteString.fromHex(compiledCodeHex)).toEither.left.map(e =>
                Error.BlueprintLoadError(
                  s"Failed to parse $title script hex: ${e.getMessage}",
                  Some(e)
                )
            )
        } yield Script.PlutusV3(scriptBytes)
    }

    /** The canonical script hash for the Rule-Based Treasury validator.
      *
      * This is loaded from the blueprint and represents the hash of the compiled script. Throws if
      * the blueprint cannot be loaded or the hash is not found.
      */
    lazy val treasuryScriptHash: ScriptHash = getScriptHash("Rule-Based Treasury Validator") match {
        case Right(hash) => hash
        case Left(error) => throw error
    }

    /** The canonical script hash for the Dispute Resolution validator.
      *
      * This is loaded from the blueprint and represents the hash of the compiled script. Throws if
      * the blueprint cannot be loaded or the hash is not found.
      */
    lazy val disputeScriptHash: ScriptHash = getScriptHash("Dispute Resolution Validator") match {
        case Right(hash) => hash
        case Left(error) => throw error
    }

    /** The canonical PlutusV3 script for the Rule-Based Treasury validator.
      *
      * This is loaded from the blueprint and represents the compiled script. Throws if the
      * blueprint cannot be loaded or the script is not found.
      */
    lazy val treasuryScript: Script = getScript("Rule-Based Treasury Validator") match {
        case Right(script) => script
        case Left(error)   => throw error
    }

    /** The canonical PlutusV3 script for the Dispute Resolution validator.
      *
      * This is loaded from the blueprint and represents the compiled script. Throws if the
      * blueprint cannot be loaded or the script is not found.
      */
    lazy val disputeScript: Script = getScript("Dispute Resolution Validator") match {
        case Right(script) => script
        case Left(error)   => throw error
    }

    /** Creates the Rule-Based Treasury script address for the given network.
      *
      * This is the canonical way to construct the treasury script address.
      */
    def mkTreasuryAddress(network: Network): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(
            scalus.cardano.ledger.ScriptHash.fromArray(treasuryScriptHash.bytes)
          ),
          delegation = Null
        )

    /** Creates the Dispute Resolution script address for the given network.
      *
      * This is the canonical way to construct the dispute resolution script address.
      */
    def mkDisputeAddress(network: Network): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(
            scalus.cardano.ledger.ScriptHash.fromArray(disputeScriptHash.bytes)
          ),
          delegation = Null
        )
}

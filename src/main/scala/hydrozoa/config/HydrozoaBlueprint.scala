package hydrozoa.config

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.ScriptHash

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

    /** Gets the script hash for the Rule-Based Treasury validator from the blueprint. */
    private def getTreasuryScriptHash: Either[Error, ScriptHash] = {
        for {
            blueprint <- loadBlueprint
            validator <- blueprint.validators
                .find(_.title == "Rule-Based Treasury Validator")
                .toRight(
                  Error.BlueprintLoadError(
                    "Rule-Based Treasury Validator not found in blueprint",
                    None
                  )
                )
            hashHex <- validator.hash.toRight(
              Error.BlueprintLoadError(
                "Rule-Based Treasury Validator hash not present in blueprint",
                None
              )
            )
        } yield ScriptHash.fromHex(hashHex)
    }

    /** Gets the script hash for the Dispute Resolution validator from the blueprint. */
    private def getDisputeScriptHash: Either[Error, ScriptHash] = {
        for {
            blueprint <- loadBlueprint
            validator <- blueprint.validators
                .find(_.title == "Dispute Resolution Validator")
                .toRight(
                  Error.BlueprintLoadError(
                    "Dispute Resolution Validator not found in blueprint",
                    None
                  )
                )
            hashHex <- validator.hash.toRight(
              Error.BlueprintLoadError(
                "Dispute Resolution Validator hash not present in blueprint",
                None
              )
            )
        } yield ScriptHash.fromHex(hashHex)
    }

    /** The canonical script hash for the Rule-Based Treasury validator.
      *
      * This is loaded from the blueprint and represents the hash of the compiled script. Throws if
      * the blueprint cannot be loaded or the hash is not found.
      */
    lazy val treasuryScriptHash: ScriptHash = getTreasuryScriptHash match {
        case Right(hash) => hash
        case Left(error) => throw error
    }

    /** The canonical script hash for the Dispute Resolution validator.
      *
      * This is loaded from the blueprint and represents the hash of the compiled script. Throws if
      * the blueprint cannot be loaded or the hash is not found.
      */
    lazy val disputeScriptHash: ScriptHash = getDisputeScriptHash match {
        case Right(hash) => hash
        case Left(error) => throw error
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

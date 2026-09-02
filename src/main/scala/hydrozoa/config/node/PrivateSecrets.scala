package hydrozoa.config.node

import cats.effect.IO
import hydrozoa.lib.StartupRefusal
import io.circe.{Json, JsonObject}
import java.nio.file.{Files, Path}
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters

/** The credentials a node needs to run, sourced from the environment rather than from
  * `private.json`.
  *
  * Splitting them out is what lets `private.json` be treated as an ordinary configuration file: it
  * can be templated, diffed, committed to a deployment repo, and handed to an operator who is not
  * trusted with the head's signing key. Everything secret arrives through the environment, which is
  * how every deployment target already wants to supply one — a Kubernetes `Secret` projected as env
  * vars, systemd's `EnvironmentFile=`, `docker run --env-file`.
  *
  * ⛔ A missing credential is a REFUSAL, not a wait. No amount of retrying conjures a signing key,
  * and a node that started without one would fail much later and much more obscurely: the signing
  * key in particular surfaces as a `BadSignature` while the node verifies its own stack-0 hard ack,
  * deep inside consensus, with nothing pointing back at the config.
  */
object PrivateSecrets:

    /** Where a credential comes from and where the decoder expects to find it.
      *
      * `path` is the JSON location the value is spliced into; the decoders are unchanged and still
      * read the field they always read.
      */
    final case class Secret(
        envName: String,
        path: List[String],
        describe: String,
        /** True for an Ed25519 signing key, whose public half sits beside it in the config and must
          * agree with it.
          */
        pairedWithVerificationKey: Boolean = false
    )

    /** The env file read when `HYDROZOA_PRIVATE_ENV` is unset: a sibling of `private.json`. */
    val defaultFileName: String = "private.env"

    private val ruleBasedPath: List[String] =
        List("nodeOperationEvacuationConfig", "ruleBasedWallet", "signingKey")

    /** Credentials that are not the peer's own signing key, whose JSON path does not depend on
      * whether this node is a head or a coil peer.
      */
    private val fixedSecrets: List[Secret] = List(
      Secret(
        "HYDROZOA_RULE_BASED_SIGNING_KEY",
        ruleBasedPath,
        "the rule-based (dispute/evacuation) wallet's signing key",
        pairedWithVerificationKey = true
      ),
      Secret("HYDROZOA_BLOCKFROST_API_KEY", List("blockfrostApiKey"), "the Blockfrost project id"),
      Secret("HYDROZOA_ADMIN_PASSWORD", List("adminPassword"), "the admin API password")
    )

    /** Parse the `KEY=value` subset systemd's `EnvironmentFile=` and `docker --env-file` share.
      *
      * Blank lines and `#` comments are skipped, a leading `export ` is tolerated so the file can
      * also be `source`d by a shell, and one layer of matching quotes is stripped. Deliberately not
      * a shell parser: no interpolation, no continuations, no `$(…)`. A credentials file that
      * executes anything is a credentials file that can surprise you.
      */
    def parseEnvFile(contents: String): Map[String, String] =
        contents.linesIterator
            .map(_.trim)
            .filter(l => l.nonEmpty && !l.startsWith("#"))
            .map(l => if l.startsWith("export ") then l.drop("export ".length).trim else l)
            .flatMap { line =>
                line.split("=", 2) match
                    case Array(k, v) if k.trim.nonEmpty =>
                        val raw = v.trim
                        val unquoted =
                            if raw.length >= 2 &&
                                ((raw.startsWith("\"") && raw.endsWith("\"")) ||
                                    (raw.startsWith("'") && raw.endsWith("'")))
                            then raw.substring(1, raw.length - 1)
                            else raw
                        Some(k.trim -> unquoted)
                    case _ => None
            }
            .toMap

    /** The peer's own signing key lives under whichever wallet field this node's role puts it in,
      * and that is the same field the decoder dispatches on.
      */
    private def ownWalletSecret(json: Json): Either[String, Secret] =
        val cursor = json.hcursor.downField("ownPeerPrivate")
        val field =
            if cursor.downField("ownHeadWallet").succeeded then Some("ownHeadWallet")
            else if cursor.downField("ownCoilWallet").succeeded then Some("ownCoilWallet")
            else None
        field
            .toRight(
              "ownPeerPrivate carries neither ownHeadWallet nor ownCoilWallet"
            )
            .map(f =>
                Secret(
                  "HYDROZOA_SIGNING_KEY",
                  List("ownPeerPrivate", f, "signingKey"),
                  "this peer's own signing key",
                  pairedWithVerificationKey = true
                )
            )

    private def at(json: Json, path: List[String]): Option[Json] =
        path.foldLeft(Option(json))((acc, k) => acc.flatMap(_.asObject).flatMap(_.apply(k)))

    /** Set `path` to `value`, creating intermediate objects as needed. */
    private def splice(json: Json, path: List[String], value: Json): Json =
        path match
            case Nil => value
            case head :: rest =>
                val obj = json.asObject.getOrElse(JsonObject.empty)
                val child = obj(head).getOrElse(Json.obj())
                Json.fromJsonObject(obj.add(head, splice(child, rest, value)))

    /** ⛔ An all-zeros placeholder is what the config ENCODER writes in place of a real signing key.
      * Finding one is expected and harmless — it is overwritten. Finding anything else means a live
      * credential is sitting in a file this split exists to make shareable.
      */
    private def isPlaceholder(j: Json): Boolean =
        j.asString.exists(s => s.isEmpty || s.forall(_ == '0'))

    /** Resolve every credential and splice it into `json`, ready for the ordinary decoders.
      *
      * Precedence is the real environment first, then the env file. That ordering is what lets a
      * Kubernetes deployment override one value without rewriting the file the rest come from.
      */
    def overlay(privateConfigPath: Path, json: Json): IO[Json] =
        for {
            envPath <- IO.delay(
              sys.env
                  .get("HYDROZOA_PRIVATE_ENV")
                  .map(Path.of(_))
                  .getOrElse(privateConfigPath.resolveSibling(defaultFileName))
            )
            fromFile <- IO.blocking {
                if Files.isReadable(envPath) then parseEnvFile(Files.readString(envPath))
                else Map.empty[String, String]
            }
            provided = fromFile ++ sys.env.view.filterKeys(fromFile.keySet ++ allEnvNames).toMap
            resolved <- IO.fromEither(
              applySecrets(json, provided, s"$privateConfigPath (credentials from $envPath)")
            )
        } yield resolved

    /** Every credential name this loader knows about. */
    def allEnvNames: Set[String] =
        (Set("HYDROZOA_SIGNING_KEY") ++ fixedSecrets.map(_.envName)).toSet

    /** The pure core: check, pair and splice, given credentials from wherever.
      *
      * Separating it from the file and environment reads is what lets the round trip be tested
      * directly — `GenerateKeyPair` writes the config and the credentials as a pair, and this
      * reassembles them.
      */
    def applySecrets(
        json: Json,
        provided: Map[String, String],
        source: String
    ): Either[StartupRefusal, Json] =
        ownWalletSecret(json).left
            .map(m => StartupRefusal(s"$source: $m"))
            .flatMap(own => resolve(own :: fixedSecrets, provided, source, json))

    private def resolve(
        secrets: List[Secret],
        fromFile: Map[String, String],
        source: String,
        json: Json
    ): Either[StartupRefusal, Json] =
        val leaked = secrets.filter(s => at(json, s.path).exists(v => !isPlaceholder(v)))
        if leaked.nonEmpty then
            Left(
              StartupRefusal(
                s"$source still contains credentials that are now read from the environment: " +
                    s"${leaked.map(_.path.mkString(".")).mkString(", ")}. Move each value out and " +
                    "delete the field, so the config file carries no secrets."
              )
            )
        else
            // A signing key is required only where its public half is: a config with no rule-based
            // wallet at all is a different (and self-reporting) problem, and demanding a key for a
            // wallet that is not there would turn the decoder's clear message into a confusing one.
            val applicable = secrets.filter(s =>
                !s.pairedWithVerificationKey || at(json, s.path.init :+ "verificationKey").isDefined
            )
            val missing = applicable.filter(s => value(s, fromFile).isEmpty)
            if missing.nonEmpty then
                Left(
                  StartupRefusal(
                    "missing credentials: " +
                        missing
                            .map(s => s"${s.envName} (${s.describe})")
                            .mkString(", ") +
                        s". Set them in the environment, or in the private.env beside $source."
                  )
                )
            else
                val spliced = applicable.foldLeft(json)((acc, s) =>
                    splice(acc, s.path, Json.fromString(value(s, fromFile).get))
                )
                pairingErrors(applicable, spliced) match
                    case Nil => Right(spliced)
                    case errs =>
                        Left(
                          StartupRefusal(
                            "credential does not match the config it is paired with: " +
                                errs.mkString("; ") +
                                ". A signing key whose public half disagrees with the config is " +
                                "not caught at load — the node signs with a key its own " +
                                "verification key does not match, and dies verifying its own " +
                                "stack-0 hard ack, deep in consensus."
                          )
                        )

    /** Every signing key must agree with the `verificationKey` sitting beside it.
      *
      * ⛔ This is the check whose absence is expensive. The two halves are supplied from different
      * places by design — the public half from the config file, the private half from the
      * environment — so nothing else in the system is positioned to notice they were paired wrong.
      */
    private def pairingErrors(secrets: List[Secret], json: Json): List[String] =
        secrets.filter(_.pairedWithVerificationKey).flatMap { s =>
            val vkeyPath = s.path.init :+ "verificationKey"
            val where = s.path.init.mkString(".")
            (at(json, s.path).flatMap(_.asString), at(json, vkeyPath).flatMap(_.asString)) match
                case (Some(skeyHex), Some(vkeyHex)) =>
                    derivePublicKeyHex(skeyHex) match
                        case Left(why) => Some(s"$where: ${s.envName} $why")
                        case Right(derived) =>
                            Option.when(!derived.equalsIgnoreCase(vkeyHex))(
                              s"$where: ${s.envName} derives verification key $derived, but the " +
                                  s"config declares $vkeyHex"
                            )
                case (_, None) => Some(s"$where: no verificationKey in the config to check against")
                case (None, _) => Some(s"$where: no signing key resolved")
        }

    /** Ed25519 public half of a 32-byte signing key, as lowercase hex. */
    private def derivePublicKeyHex(skeyHex: String): Either[String, String] =
        val cleaned = skeyHex.trim
        if cleaned.length != 64 || !cleaned.forall(c => "0123456789abcdefABCDEF".contains(c)) then
            Left(s"is not 32 bytes of hex (got ${cleaned.length} characters)")
        else
            try
                val bytes = cleaned.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
                val pub = Ed25519PrivateKeyParameters(bytes, 0).generatePublicKey().getEncoded
                Right(pub.map("%02x".format(_)).mkString)
            catch case e: Exception => Left(s"could not be read as an Ed25519 key: ${e.getMessage}")

    private def value(s: Secret, provided: Map[String, String]): Option[String] =
        provided.get(s.envName).filter(_.nonEmpty)

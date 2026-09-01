package hydrozoa.config.node

import io.circe.{Json, parser}
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.scalatest.funsuite.AnyFunSuite

class PrivateSecretsTest extends AnyFunSuite {

    private def skey(digit: Char): String = digit.toString * 64

    private def vkeyOf(skeyHex: String): String =
        Ed25519PrivateKeyParameters(
          skeyHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray,
          0
        ).generatePublicKey().getEncoded.map("%02x".format(_)).mkString

    /** A config in the shape the split produces: public halves present, secrets absent. */
    private def config(ownVkey: String, ruleVkey: String): Json =
        parser
            .parse(s"""{
              "ownPeerPrivate": { "ownHeadWallet": { "verificationKey": "$ownVkey" } },
              "nodeOperationEvacuationConfig": {
                "ruleBasedWallet": { "verificationKey": "$ruleVkey" }
              },
              "httpPort": "8080"
            }""")
            .fold(e => fail(s"bad fixture: $e"), identity)

    private def credentials(own: String, rule: String): Map[String, String] = Map(
      "HYDROZOA_SIGNING_KEY" -> own,
      "HYDROZOA_RULE_BASED_SIGNING_KEY" -> rule,
      "HYDROZOA_BLOCKFROST_API_KEY" -> "previewXXXX",
      "HYDROZOA_ADMIN_PASSWORD" -> "hunter2"
    )

    test("a complete, matching credential set is spliced into the config") {
        val json = config(vkeyOf(skey('1')), vkeyOf(skey('2')))
        val out = PrivateSecrets
            .applySecrets(json, credentials(skey('1'), skey('2')), "test")
            .fold(e => fail(s"unexpectedly refused: ${e.reason}"), identity)

        val c = out.hcursor
        assert(
          c.downField("ownPeerPrivate")
              .downField("ownHeadWallet")
              .downField("signingKey")
              .as[String] == Right(skey('1'))
        )
        assert(c.downField("blockfrostApiKey").as[String] == Right("previewXXXX"))
        assert(c.downField("adminPassword").as[String] == Right("hunter2"))
    }

    test("a missing credential is refused, and the message names it") {
        val json = config(vkeyOf(skey('1')), vkeyOf(skey('2')))
        val refusal = PrivateSecrets
            .applySecrets(
              json,
              credentials(skey('1'), skey('2')) - "HYDROZOA_ADMIN_PASSWORD",
              "test"
            )
            .swap
            .fold(_ => fail("expected a refusal"), identity)
        assert(refusal.reason.contains("HYDROZOA_ADMIN_PASSWORD"))
    }

    // The check that earns this module its place: the two halves of a keypair now arrive from
    // different places, so nothing else in the system is positioned to notice they were paired
    // wrong. Unchecked it surfaces as a BadSignature deep in consensus.
    test("a signing key that does not match its verification key is refused") {
        val json = config(vkeyOf(skey('1')), vkeyOf(skey('2')))
        val refusal = PrivateSecrets
            .applySecrets(json, credentials(skey('9'), skey('2')), "test")
            .swap
            .fold(_ => fail("expected a refusal"), identity)
        assert(refusal.reason.contains("derives verification key"))
        assert(refusal.reason.contains("ownPeerPrivate.ownHeadWallet"))
    }

    test("the rule-based wallet is paired too, not just the peer's own") {
        val json = config(vkeyOf(skey('1')), vkeyOf(skey('2')))
        val refusal = PrivateSecrets
            .applySecrets(json, credentials(skey('1'), skey('9')), "test")
            .swap
            .fold(_ => fail("expected a refusal"), identity)
        assert(refusal.reason.contains("ruleBasedWallet"))
    }

    // The whole point of the split is that the config file becomes shareable. A live key left in
    // it would defeat that silently, so it is refused rather than overwritten.
    test("a credential left behind in the config is refused") {
        val json = config(vkeyOf(skey('1')), vkeyOf(skey('2')))
            .deepMerge(Json.obj("blockfrostApiKey" -> Json.fromString("previewLEFTBEHIND")))
        val refusal = PrivateSecrets
            .applySecrets(json, credentials(skey('1'), skey('2')), "test")
            .swap
            .fold(_ => fail("expected a refusal"), identity)
        assert(refusal.reason.contains("blockfrostApiKey"))
    }

    // The encoder writes an all-zeros stand-in where a signing key would go. That is not a leak
    // and must not be treated as one -- it is simply overwritten.
    test("the encoder's all-zeros placeholder is overwritten, not treated as a leak") {
        val json = config(vkeyOf(skey('1')), vkeyOf(skey('2'))).deepMerge(
          Json.obj(
            "ownPeerPrivate" -> Json.obj(
              "ownHeadWallet" -> Json.obj("signingKey" -> Json.fromString("0" * 64))
            )
          )
        )
        val out = PrivateSecrets
            .applySecrets(json, credentials(skey('1'), skey('2')), "test")
            .fold(e => fail(s"unexpectedly refused: ${e.reason}"), identity)
        assert(
          out.hcursor
              .downField("ownPeerPrivate")
              .downField("ownHeadWallet")
              .downField("signingKey")
              .as[String] == Right(skey('1'))
        )
    }

    test("a config with no rule-based wallet does not demand its key") {
        val json = parser
            .parse(s"""{
              "ownPeerPrivate": { "ownHeadWallet": { "verificationKey": "${vkeyOf(skey('1'))}" } }
            }""")
            .fold(e => fail(s"bad fixture: $e"), identity)
        val creds = Map(
          "HYDROZOA_SIGNING_KEY" -> skey('1'),
          "HYDROZOA_BLOCKFROST_API_KEY" -> "previewXXXX",
          "HYDROZOA_ADMIN_PASSWORD" -> "hunter2"
        )
        assert(PrivateSecrets.applySecrets(json, creds, "test").isRight)
    }

    test("a malformed signing key is refused rather than parsed into nonsense") {
        val json = config(vkeyOf(skey('1')), vkeyOf(skey('2')))
        val refusal = PrivateSecrets
            .applySecrets(json, credentials("nothex", skey('2')), "test")
            .swap
            .fold(_ => fail("expected a refusal"), identity)
        assert(refusal.reason.contains("32 bytes of hex"))
    }

    test("the env file parser handles comments, quotes, blanks and export") {
        val parsed = PrivateSecrets.parseEnvFile("""
          |# a comment
          |
          |HYDROZOA_SIGNING_KEY=abc123
          |export HYDROZOA_ADMIN_PASSWORD="quoted value"
          |HYDROZOA_BLOCKFROST_API_KEY='single'
          |  SPACED  =  padded
          |not a pair
          |""".stripMargin)

        assert(parsed("HYDROZOA_SIGNING_KEY") == "abc123")
        assert(parsed("HYDROZOA_ADMIN_PASSWORD") == "quoted value")
        assert(parsed("HYDROZOA_BLOCKFROST_API_KEY") == "single")
        assert(parsed("SPACED") == "padded")
        assert(!parsed.contains("not a pair"))
    }

    test("a value containing '=' survives the split") {
        assert(PrivateSecrets.parseEnvFile("K=a=b=c")("K") == "a=b=c")
    }
}

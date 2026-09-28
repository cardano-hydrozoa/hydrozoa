package hydrozoa.app

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.app.cli.DemoConfig
import hydrozoa.config.node.PrivateSecrets
import hydrozoa.multisig.consensus.peer.PeerWallet
import java.nio.file.{Files, Path}
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.scalatest.funsuite.AnyFunSuite
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString

/** The CLI commands that sign without a full [[hydrozoa.config.node.NodeConfig.load]] —
  * `submit-l2-tx` ([[DemoConfig.readWallet]]) and `deploy-scripts-and-g2-setup`
  * ([[DeployScriptsAndG2Setup.readWallet]]) — must read the signing key the way the node does: from
  * the environment or the `private.env` beside `private.json`, never from `private.json` itself,
  * which `keygen` now writes with only the verification key.
  */
class CliWalletCredentialsTest extends AnyFunSuite:

    private val readers: List[(String, Path => IO[PeerWallet])] = List(
      "DemoConfig.readWallet" -> DemoConfig.readWallet,
      "DeployScriptsAndG2Setup.readWallet" -> DeployScriptsAndG2Setup.readWallet
    )

    private val skeyHex: String = "1" * 64

    private val vkeyHex: String =
        Ed25519PrivateKeyParameters(
          skeyHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray,
          0
        ).generatePublicKey().getEncoded.map("%02x".format(_)).mkString

    /** A `private.json` in the shape `keygen` writes: the public half only, unless `extra` adds
      * more fields to the wallet object.
      */
    private def writeConfig(extra: String = "", env: Option[String] = None): Path =
        val dir = Files.createTempDirectory("hydrozoa-cli-wallet-")
        val path = dir.resolve("private.json")
        Files.writeString(
          path,
          s"""{ "ownPeerPrivate": { "ownHeadWallet": { "verificationKey": "$vkeyHex"$extra } } }"""
        ): Unit
        env.foreach(e => Files.writeString(dir.resolve(PrivateSecrets.defaultFileName), e): Unit)
        path

    // The overlay prefers the real environment; a key set there would mask what these cases test.
    private def assumeCleanEnv(): Unit =
        assume(
          !sys.env.contains("HYDROZOA_PRIVATE_ENV") && !sys.env.contains("HYDROZOA_SIGNING_KEY"),
          "HYDROZOA_PRIVATE_ENV / HYDROZOA_SIGNING_KEY set in the test environment"
        ): Unit

    readers.foreach { (name, read) =>
        test(s"$name reads the signing key from the private.env beside private.json") {
            assumeCleanEnv()
            val path = writeConfig(env = Some(s"HYDROZOA_SIGNING_KEY=$skeyHex\n"))
            val wallet = read(path).unsafeRunSync()
            assert(
              wallet.exportVerificationKey ==
                  VerificationKey.unsafeFromByteString(ByteString.fromHex(vkeyHex))
            ): Unit
        }

        test(s"$name refuses a config with no signing key anywhere, naming the variable") {
            assumeCleanEnv()
            val path = writeConfig()
            val err = read(path).attempt.unsafeRunSync().swap.getOrElse(fail("expected a refusal"))
            assert(err.getMessage.contains("HYDROZOA_SIGNING_KEY"), err.getMessage): Unit
        }

        test(s"$name refuses a signing key left in private.json, as the node does") {
            assumeCleanEnv()
            val path = writeConfig(
              extra = s""", "signingKey": "$skeyHex"""",
              env = Some(s"HYDROZOA_SIGNING_KEY=$skeyHex\n")
            )
            val err = read(path).attempt.unsafeRunSync().swap.getOrElse(fail("expected a refusal"))
            assert(err.getMessage.contains("ownPeerPrivate.ownHeadWallet.signingKey")): Unit
        }
    }

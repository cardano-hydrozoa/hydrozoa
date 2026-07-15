package hydrozoa.lib.cardano.wallet

import com.bloxbean.cardano.client.cip.cip30.{CIP30DataSigner, DataSignature}
import scala.util.Try
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString

/** Verify a CIP-30 `signData()` COSE_Sign1 — the `(coseKey, coseSignature)` CBOR-hex pair that
  * [[WalletModule.signCoseCip30]] produces. Keyless: everything needed to verify travels inside the
  * COSE blobs (the public key lives in the COSE key), so no wallet is required.
  */
object Cip30Verify {

    enum Error(val message: String):
        case SignatureMismatch extends Error("COSE signature verification failed")
        case VerificationKeyParsingFailure
            extends Error("could not parse the verification key from the COSE key")

    /** Verify the `(coseKey, coseSignature)` pair. On success returns the verification key carried
      * in the COSE key and the payload the signature covers.
      *
      * It does **not** judge the payload or the key — the caller decides what the signed payload
      * must be (e.g. `hash(l2Payload)`) and whether the key is an authorized signer.
      */
    def verify(
        coseKeyCborHex: String,
        coseSignatureCborHex: String
    ): Either[Error, (VerificationKey, ByteString)] = {
        val ds = DataSignature(coseSignatureCborHex, coseKeyCborHex)
        for {
            _ <- Either.cond(CIP30DataSigner.INSTANCE.verify(ds), (), Error.SignatureMismatch)
            // Extract the public key from COSE key parameter -2 (x-coordinate for OKP/Ed25519 keys).
            vKey <- Try(
              VerificationKey.unsafeFromArray(ds.coseKey().otherHeaderAsBytes(-2))
            ).toEither.left.map(_ => Error.VerificationKeyParsingFailure)
            payload = ByteString.fromArray(ds.coseSign1().payload())
        } yield (vKey, payload)
    }
}

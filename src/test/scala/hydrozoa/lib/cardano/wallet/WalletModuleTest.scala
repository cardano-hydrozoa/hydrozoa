package hydrozoa.lib.cardano.wallet

import com.bloxbean.cardano.client.cip.cip30.{CIP30DataSigner, DataSignature}
import com.bloxbean.cardano.client.crypto.bip32.HdKeyGenerator
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.util.HexUtil
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.crypto.ed25519.{SigningKey as ScalusSigningKey, VerificationKey as ScalusVerificationKey}

object WalletModuleTest extends Properties("WalletModule") {

    // 32-byte entropy → BIP-39 24-word mnemonic space; HdKeyGenerator derives the same HdKeyPair
    // shape TestPeers uses in production tests.
    private val genEntropy: Gen[Array[Byte]] =
        Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])

    // Non-empty so bloxbean's payload check is exercised.
    private val genPayload: Gen[Array[Byte]] =
        Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbitrary[Byte])

    private def bloxbeanKeys(entropy: Array[Byte]): (HdPublicKey, HdPrivateKey) =
        val kp = new HdKeyGenerator().getRootKeyPairFromEntropy(entropy)
        (kp.getPublicKey, kp.getPrivateKey)

    private def scalusKeys(entropy: Array[Byte]): (ScalusVerificationKey, ScalusSigningKey) =
        // Scalus uses standard 32-byte Ed25519 keys, not BIP-32 extended. Take the first 32 bytes
        // of entropy as the Ed25519 seed and derive the matching public key via BouncyCastle.
        val seed = entropy.take(32)
        val privParams = new Ed25519PrivateKeyParameters(seed, 0)
        val pubBytes = privParams.generatePublicKey().getEncoded
        (ScalusVerificationKey.unsafeFromArray(pubBytes), ScalusSigningKey.unsafeFromArray(seed))

    private def asDataSignature(signed: Cip30SignedData): DataSignature =
        new DataSignature(signed.coseSignatureCborHex, signed.coseKeyCborHex)

    // Property: bloxbean's own verify accepts what we produce — round-trip through the CIP-30
    // signData/verify contract.
    val _ = property("BloxBean.signCoseCip30 output validates via CIP30DataSigner.verify") =
        forAll(genEntropy, genPayload) { (entropy, payload) =>
            val (vk, sk) = bloxbeanKeys(entropy)
            val signed = WalletModule.BloxBean.signCoseCip30(payload, vk, sk)
            CIP30DataSigner.INSTANCE.verify(asDataSignature(signed))
        }

    val _ = property("Scalus.signCoseCip30 output validates via CIP30DataSigner.verify") =
        forAll(genEntropy, genPayload) { (entropy, payload) =>
            val (vk, sk) = scalusKeys(entropy)
            val signed = WalletModule.Scalus.signCoseCip30(payload, vk, sk)
            CIP30DataSigner.INSTANCE.verify(asDataSignature(signed))
        }

    // Property: coseKey's COSE key parameter -2 (OKP curve x-coordinate) is exactly the wallet's
    // public key — the field Cip30Verify.verify reads to recover the signer key.
    val _ = property(
      "BloxBean.signCoseCip30 embeds the public key in the coseKey X header (param -2)"
    ) = forAll(genEntropy, genPayload) { (entropy, payload) =>
        val (vk, sk) = bloxbeanKeys(entropy)
        val signed = WalletModule.BloxBean.signCoseCip30(payload, vk, sk)
        val ds = asDataSignature(signed)
        val extractedPubKey = ds.coseKey().otherHeaderAsBytes(-2L)
        extractedPubKey.sameElements(vk.getKeyData)
    }

    // Property: Cip30Verify.verify accepts a signCoseCip30 output and recovers the exact payload +
    // signer key. This is the sign↔verify contract DepositPreScreening relies on.
    val _ = property("Cip30Verify.verify recovers the payload + signer vkey (BloxBean)") =
        forAll(genEntropy, genPayload) { (entropy, payload) =>
            val (vk, sk) = bloxbeanKeys(entropy)
            val signed = WalletModule.BloxBean.signCoseCip30(payload, vk, sk)
            Cip30Verify.verify(signed.coseKeyCborHex, signed.coseSignatureCborHex) match {
                case Right((vKey, signedPayload)) =>
                    vKey == ScalusVerificationKey.unsafeFromArray(vk.getKeyData) &&
                    signedPayload.bytes.sameElements(payload)
                case Left(_) => false
            }
        }

    val _ = property("Cip30Verify.verify recovers the payload + signer vkey (Scalus)") =
        forAll(genEntropy, genPayload) { (entropy, payload) =>
            val (vk, sk) = scalusKeys(entropy)
            val signed = WalletModule.Scalus.signCoseCip30(payload, vk, sk)
            Cip30Verify.verify(signed.coseKeyCborHex, signed.coseSignatureCborHex) match {
                case Right((vKey, signedPayload)) =>
                    vKey == vk && signedPayload.bytes.sameElements(payload)
                case Left(_) => false
            }
        }

    // Leak check: the private-key hex must not appear as a substring in either the coseKey or
    // the coseSignature CBOR hex. Collision on 64+ hex chars is astronomical; a positive substring
    // hit would be a real leak.
    val _ = property(
      "BloxBean.signCoseCip30 does not leak the private key hex into coseKey/coseSignature"
    ) = forAll(genEntropy, genPayload) { (entropy, payload) =>
        val (vk, sk) = bloxbeanKeys(entropy)
        val signed = WalletModule.BloxBean.signCoseCip30(payload, vk, sk)
        val privHex = HexUtil.encodeHexString(sk.getKeyData).toLowerCase
        val coseKeyHex = signed.coseKeyCborHex.toLowerCase
        val coseSigHex = signed.coseSignatureCborHex.toLowerCase
        !coseKeyHex.contains(privHex) && !coseSigHex.contains(privHex)
    }

    val _ = property(
      "Scalus.signCoseCip30 does not leak the private key hex into coseKey/coseSignature"
    ) = forAll(genEntropy, genPayload) { (entropy, payload) =>
        val (vk, sk) = scalusKeys(entropy)
        val signed = WalletModule.Scalus.signCoseCip30(payload, vk, sk)
        val privHex = HexUtil.encodeHexString(sk.bytes).toLowerCase
        val coseKeyHex = signed.coseKeyCborHex.toLowerCase
        val coseSigHex = signed.coseSignatureCborHex.toLowerCase
        !coseKeyHex.contains(privHex) && !coseSigHex.contains(privHex)
    }
}

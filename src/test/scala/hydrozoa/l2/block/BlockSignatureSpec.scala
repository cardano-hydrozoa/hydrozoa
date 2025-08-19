package hydrozoa.l2.block

import hydrozoa.infra.encodeHex
import hydrozoa.l2.block.BlockTypeL2.Minor
import hydrozoa.l2.commitment.infG2hex
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.Alice
import scalus.builtin.Builtins.verifyEd25519Signature
import scalus.builtin.ByteString

class BlockSignatureSpec extends munit.ScalaCheckSuite {

    test("sign-verify round-trip") {
        val bh = BlockHeader(42, Minor, 0, 1, 0, infG2hex)
        println(bh)
        val sBh = mkBlockHeaderSignatureMessage(bh)
        println(encodeHex(sBh))
        val wallet = TestPeer.mkWallet(Alice)
        val signature = wallet.createEd25519Signature(sBh)
        println(encodeHex(signature.untagged))

        val vk = wallet.exportVerificationKeyBytes
        val result = verifyEd25519Signature(
          vk.bytes,
          ByteString.fromArray(IArray.genericWrapArray(sBh).toArray),
          ByteString.fromArray(IArray.genericWrapArray(signature.untagged).toArray)
        )
        println(result)
        assert(result, "Validation fails")
    }
}

package hydrozoa.head.network

import com.bloxbean.cardano.client.crypto.{KeyGenUtil, Keys, VerificationKey}

class MockHydrozoaNetwork extends HydrozoaNetwork {
  override def participantsKeys(): Set[VerificationKey] = {
    // Fake keys
    val keys1: Keys = KeyGenUtil.generateKey
    val keys2: Keys = KeyGenUtil.generateKey
    val vkey1: VerificationKey = keys1.getVkey
    val vkey2: VerificationKey = keys2.getVkey
    Set(vkey1, vkey2)
  }
}

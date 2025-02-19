package hydrozoa.head.network

import com.bloxbean.cardano.client.crypto.{KeyGenUtil, Keys, SecretKey, VerificationKey}

class MockHydrozoaNetwork extends HydrozoaNetwork {

  private val keys1: Keys = KeyGenUtil.generateKey
  private val keys2: Keys = KeyGenUtil.generateKey

  override def participantsKeys(): Set[VerificationKey] = {
    val vkey1: VerificationKey = keys1.getVkey
    val vkey2: VerificationKey = keys2.getVkey
    Set(vkey1, vkey2)
  }

  override def participantsSigningKeys(): Set[SecretKey] = Set(keys1.getSkey, keys2.getSkey)
}
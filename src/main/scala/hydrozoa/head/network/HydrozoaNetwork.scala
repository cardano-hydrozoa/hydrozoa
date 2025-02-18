package hydrozoa.head.network

import com.bloxbean.cardano.client.crypto.VerificationKey

trait HydrozoaNetwork {
  /**
   * @return verification keys for known participants
   */
  def participantsKeys(): Set[VerificationKey]
}

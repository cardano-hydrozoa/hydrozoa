package hydrozoa.head.network

import com.bloxbean.cardano.client.crypto.{SecretKey, VerificationKey}

trait HydrozoaNetwork {
  /**
   * @return verification keys for known participants
   */
  def participantsKeys(): Set[VerificationKey]

  // FIXME: temporary
  def participantsSigningKeys(): Set[SecretKey]
}

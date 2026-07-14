package hydrozoa.lib.cardano.wallet

/** CIP-30 `signData()` output: COSE_Key + COSE_Sign1 as CBOR hex. Constructor is `private[wallet]`
  * — only [[WalletModule]] impls produce values.
  */
final case class Cip30SignedData private[wallet] (
    coseKeyCborHex: String,
    coseSignatureCborHex: String,
)

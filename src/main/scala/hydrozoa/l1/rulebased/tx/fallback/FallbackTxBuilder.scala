package hydrozoa.l1.rulebased.tx.fallback

import hydrozoa.*

// Fallback AKA post-dated transition to the rule-based regime.

trait FallbackTxBuilder {

    /** @param recipe
      * @return
      */
    def buildFallbackTxDraft(recipe: FallbackTxRecipe): Either[String, TxL1]
}

case class FallbackTxRecipe(
    // Since it's an AOT-transaction, the treasury output it spends doesn't exist
    // by the time of building.
    //
    // So `multisigTx` is used to give the builder enough information about the treasury utxo.
    // We use tx not the output since it's easier in terms of types though arguably
    // violate POLA.
    //
    // It should be a multisig transaction that has one output to the head's address
    // that contains the head's beacon token and a multisig treasury datum.
    //
    // The same output will be used to pay fallback transaction's fees.
    // It can be:
    //   -- an initialization tx
    //   -- a settlement tx
    multisigTx: TxL1,

    // Bech32 address of the rule-based regime's treasury script.
    treasuryScript: AddressBechL1,

    // Bech32 address of the rule-based regime's resolution script.
    disputeScript: AddressBechL1,

    // Voting duration from head parameters.
    // TODO: see https://github.com/cardano-hydrozoa/hydrozoa/issues/129
    votingDuration: PosixTime,

    // Head's peers.
    peers: List[VerificationKeyBytes],
    headAddressBech32: AddressBechL1,
    headNativeScript: NativeScript,
    headMintingPolicy: CurrencySymbol
)

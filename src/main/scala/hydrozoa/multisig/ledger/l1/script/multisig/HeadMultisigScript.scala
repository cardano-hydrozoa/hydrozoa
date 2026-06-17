package hydrozoa.multisig.ledger.l1.script.multisig

import hydrozoa.config.head.peers.HeadPeers
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Timelock.{AllOf, MOf, Signature}
import scalus.cardano.txbuilder.NativeScriptWitness
import scalus.cardano.txbuilder.ScriptSource.{NativeScriptAttached, NativeScriptValue}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.blake2b_224

case class HeadMultisigScript(private val script0: Script.Native) {
    val script: Script.Native = script0
    def mkAddress(network: Network = Mainnet): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(script.scriptHash),
          delegation = Null
        )
    val policyId: PolicyId = script.scriptHash

    /** Expected signers for fee / witness-set sizing: every head peer plus exactly `coilQuorum`
      * coil placeholders (the leading coil leaves of the `MOf` branch). The actual coil signer
      * subset is chosen at aggregation; only the COUNT (`nHeadPeers + coilQuorum`) feeds fee
      * estimation, so any `coilQuorum` placeholders suffice here.
      *
      * NOTE: because this is a [[Set]], the traversal order WILL NOT be the same as
      * [[headPeers.headPeerVKeys]].
      */
    val requiredSigners: Set[AddrKeyHash] = {
        val all = script.script.asInstanceOf[Timelock.AllOf].scripts
        val headKeyHashes = all.collect { case s: Signature => s.keyHash }
        val coilKeyHashes = all.collectFirst { case m: MOf => m }.toList.flatMap { m =>
            m.scripts.collect { case s: Signature => s.keyHash }.take(m.m)
        }
        (headKeyHashes ++ coilKeyHashes).toSet
    }
    val numSigners: Int = requiredSigners.toSeq.size

    def checkSigners(signers: Set[AddrKeyHash]): Boolean = signers == requiredSigners

    // use when the multisig witness utxo id not available
    val witnessValue: NativeScriptWitness = NativeScriptWitness(
      scriptSource = NativeScriptValue(script)
    )

    // use when referencing the multisig witness utxo
    // or after [[witnessValue]] has been used within a tx
    val witnessAttached: NativeScriptWitness = NativeScriptWitness(
      scriptSource = NativeScriptAttached
    )

}

object HeadMultisigScript:
    /** Build the head's multisig native script. With no coil peers it is `AllOf(headSignatures)` —
      * byte-identical to a coil-free head. With coil peers it appends a single
      * `MOf(coilQuorum, coilSignatures)` branch, so every effect tx must be signed by all head
      * peers and at least `coilQuorum` coil peers — the one threshold governing minting, treasury /
      * regime spend, and the head address alike.
      */
    def apply(
        headPeers: HeadPeers.Section,
        coilPeerVKeys: List[VerificationKey] = List.empty,
        coilQuorum: Int = 0
    ): HeadMultisigScript = {
        def mkSignature(vkey: VerificationKey): Signature =
            Signature(AddrKeyHash(blake2b_224(vkey)))
        val headLeaves: List[Timelock] = headPeers.headPeerVKeys.toList.map(mkSignature)
        val scripts: IndexedSeq[Timelock] =
            if coilPeerVKeys.isEmpty then headLeaves.toIndexedSeq
            else {
                val coilBranch = MOf(coilQuorum, coilPeerVKeys.map(mkSignature).toIndexedSeq)
                (headLeaves :+ coilBranch).toIndexedSeq
            }
        HeadMultisigScript(Script.Native(AllOf(scripts)))
    }

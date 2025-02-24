package hydrozoa.head.network

import hydrozoa.head.*
import hydrozoa.head.l1.Cardano
import hydrozoa.head.l1.txbuilder.{InitTxRecipe, TxBuilder}
import hydrozoa.head.multisig.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}

class MockHydrozoaNetwork(
    txBuilder: TxBuilder,
    cardano: Cardano,
    theLastVerificationKey: ParticipantVerificationKey // this is the key of the only "real" node
) extends HydrozoaNetwork {

    private val keys1 = genNodeKey()
    private val keys2 = genNodeKey()

    override def participantsKeys(): Set[ParticipantVerificationKey] =
        Set(keys1, keys2).map(_._2)

    override def reqInit(req: ReqInit): Set[TxKeyWitness] = {
        // Head's verification keys
        val vKeys = Set(keys1, keys2).map(_._2) + theLastVerificationKey

        // Native script, head address, and token
        val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network())
        val beaconTokenName = mkBeaconTokenName(req.txId, req.txIx)

        // Recipe to build init tx
        val initTxRecipe = InitTxRecipe(
          headAddress,
          req.txId,
          req.txIx,
          req.amount,
          headNativeScript,
          beaconTokenName
        )

        val tx: L1Tx = txBuilder.mkInitDraft(initTxRecipe) match
            case Right(tx) => tx
            case Left(err) => println(err); ???

        val wit1: TxKeyWitness = signTx(tx, keys1._1)
        val wit2: TxKeyWitness = signTx(tx, keys2._1)
        Set(wit1, wit2)
    }
}

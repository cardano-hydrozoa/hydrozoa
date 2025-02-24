package hydrozoa.head

import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.head.l1.Cardano
import hydrozoa.head.l1.txbuilder.{InitTxRecipe, TxBuilder}
import hydrozoa.head.multisig.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.head.network.{HydrozoaNetwork, ReqInit}
import hydrozoa.head.wallet.Wallet
import hydrozoa.logging.LoggingService

class Node(
    ownKeys: (ParticipantSecretKey, ParticipantVerificationKey),
    network: HydrozoaNetwork,
    cardano: Cardano,
    wallet: Wallet,
    txBuilder: TxBuilder,
    logging: LoggingService
):

    def initializeHead(amount: Long, txId: TxId, txIx: TxIx): Either[String, String] = {
        logging.logInfo(
          "Trying to initialize the head with seed utxo " + txId + "#" + txIx + ", amount: " + amount + "ADA."
        )

        // FIXME: check head/node status

        // Head's verification keys
        val vKeys = network.participantsKeys() + ownKeys._2

        // Native script, head address, and token
        val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network())
        val beaconTokenName = mkBeaconTokenName(txId, txIx)

        // Recipe to build init tx
        val initTxRecipe = InitTxRecipe(
          headAddress,
          txId,
          txIx,
          amount,
          headNativeScript,
          beaconTokenName
        )

        val txDraft = txBuilder.mkInitDraft(initTxRecipe) match
            case Right(v)  => v
            case Left(err) => return Left(err)

        val ownWit: TxKeyWitness = signTx(txDraft, ownKeys._1)

        val peersWits: Set[TxKeyWitness] = network.reqInit(ReqInit(txId, txIx, amount))
        // FIXME: broadcast ownWit

        val userWit = wallet.sign(txDraft)

        val wits = peersWits + ownWit + userWit

        println(wits)

        val initTx: L1Tx = wits.foldLeft(txDraft)(addWitness)

        println(Transaction.deserialize(initTx.bytes).serializeToHex())

        cardano.submit(initTx).map(_.hash)
    }

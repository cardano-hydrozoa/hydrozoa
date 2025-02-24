package hydrozoa.node.server

import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.typesafe.scalalogging.Logger
import hydrozoa.l1.Cardano
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l2.consensus.network.{HydrozoaNetwork, ReqInit}
import hydrozoa.*
import hydrozoa.infra.{addWitness, signTx}
import hydrozoa.l1.wallet.Wallet

class Node(
    ownKeys: (ParticipantSecretKey, ParticipantVerificationKey),
    network: HydrozoaNetwork,
    cardano: Cardano,
    wallet: Wallet,
    txBuilder: InitTxBuilder,
    log: Logger
):

    def initializeHead(amount: Long, txId: TxId, txIx: TxIx): Either[String, String] = {
        log.warn(s"Init the head with seed ${txId.hash}#${txIx.ix}, amount $amount ADA")

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

        val initTx: L1Tx = wits.foldLeft(txDraft)(addWitness)

        log.whenInfoEnabled {
            log.info("Init tx: " + Transaction.deserialize(initTx.bytes).serializeToHex())
        }

        cardano.submit(initTx).map(_.hash)
    }

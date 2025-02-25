package hydrozoa.node.server

import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{addWitness, signTx}
import hydrozoa.l1.Cardano
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.wallet.Wallet
import hydrozoa.l2.consensus.network.{HydrozoaNetwork, ReqInit, ReqRefundLater}
import hydrozoa.node.server.DepositError
import hydrozoa.*

class Node(
    ownKeys: (ParticipantSecretKey, ParticipantVerificationKey),
    network: HydrozoaNetwork,
    cardano: Cardano,
    wallet: Wallet,
    txBuilder: InitTxBuilder,
    log: Logger
):

    def initializeHead(amount: Long, txId: TxId, txIx: TxIx): Either[InitializeError, TxId] = {
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

        cardano.submit(initTx)
    }

    def deposit(r: DepositRequest): Either[DepositError, DepositResponse] = {

        def mkDepositTx(r: Any) = ???
        def mkRefundTx(a: Any, b: Any) = ???

        // Build a deposit transaction as a courtesy of Hydrozoa (no signature)
        val depositTx: L1Tx = mkDepositTx(r)
        val index: Int = 0

        // Build a post-dated refund tx
        // TODO: Add a comment to explain how it guarantees a deposit cannot be stolen by peers
        val refundTxDraft: L1Tx = mkRefundTx(depositTx, index)

        // Own signature
        val ownWit: TxKeyWitness = signTx(refundTxDraft, ownKeys._1)

        // ReqRefundLater
        val peersWits: Set[TxKeyWitness] = network.reqRefundLater(ReqRefundLater(depositTx, index))
        // FIXME: broadcast ownWit

        val wits = peersWits + ownWit

        val refundTx: L1Tx = wits.foldLeft(refundTxDraft)(addWitness)

        Right(DepositResponse(refundTx, (???, ???)))
    }

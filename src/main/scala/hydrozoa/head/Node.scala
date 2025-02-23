package hydrozoa.head

import hydrozoa.head.l1.Cardano
import hydrozoa.head.l1.txbuilder.{InitTxRecipe, TxBuilder}
import hydrozoa.head.multisig.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.head.network.{HydrozoaNetwork, ReqInit}
import hydrozoa.head.wallet.Wallet
import hydrozoa.logging.LoggingService


class Node(
            network: HydrozoaNetwork,
            cardano: Cardano,
            wallet: Wallet,
            txBuilder: TxBuilder,
            logging: LoggingService
          ):

  private val ownKeys = genNodeKey()

  def initializeHead(amount: Long, txId: TxId, txIx: TxIx): Either[String, String] = {
    logging.logInfo("Trying to initialize the head with seed utxo " + txId + "#" + txIx + ", amount: " + amount + "ADA.")

    // FIXME: check head/node status

    // Head's verification keys
    val vKeys = network.participantsKeys() + ownKeys._2

    // Native script, head address, and token
    val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network())
    val beaconTokenName = mkBeaconTokenName(txId, txIx)

    // Recipe to build init tx
    val initTxRecipe = InitTxRecipe(
      headAddress,
      txId, txIx, amount,
      headNativeScript, beaconTokenName
    )

    for
      txDraft <- txBuilder.mkInitDraft(initTxRecipe)

      ownWit: TxKeyWitness = signTx(txDraft, ownKeys._1)

      peersWits: Set[TxKeyWitness] = network.reqInit(ReqInit(txId, txIx, amount))
      // FIXME: broadcast ownWit

      userWit = wallet.sign(txDraft)

      initTx: L1Tx = (peersWits + ownWit + userWit).foldLeft(txDraft)(addWitness)

      ret <- cardano.submit(initTx)

    yield ret.hash
  }

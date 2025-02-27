package hydrozoa.node.server

import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.Cardano
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.MultisigTxs.DepositTx
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.wallet.Wallet
import hydrozoa.l2.consensus.network.{HydrozoaNetwork, ReqInit, ReqRefundLater}
import hydrozoa.node.server.DepositError
import scalus.prelude.Maybe

class Node(
    headStateManager: HeadStateManager,
    ownKeys: (ParticipantSecretKey, ParticipantVerificationKey),
    network: HydrozoaNetwork,
    cardano: Cardano,
    wallet: Wallet,
    initTxBuilder: InitTxBuilder,
    depositTxBuilder: DepositTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    log: Logger
):

    def initializeHead(amount: Long, txId: TxId, txIx: TxIx): Either[InitializeError, TxId] = {
        log.warn(s"Init the head with seed ${txId.hash}#${txIx.ix}, amount $amount ADA")

        // FIXME: check head/node status

        // Make a recipe to build init tx

        // Head's verification keys
        val vKeys = network.participantsKeys() + ownKeys._2

        // Native script, head address, and token
        val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network())
        val beaconTokenName = mkBeaconTokenName(txId, txIx)

        val initTxRecipe = InitTxRecipe(
          headAddress,
          txId,
          txIx,
          amount,
          headNativeScript,
          beaconTokenName
        )

        // Builds and balance Cardano tx
        val txDraft = initTxBuilder.mkInitDraft(initTxRecipe) match
            case Right(v)  => v
            case Left(err) => return Left(err)

        val ownWit: TxKeyWitness = signTx(txDraft, ownKeys._1)

        val peersWits: Set[TxKeyWitness] = network.reqInit(ReqInit(txId, txIx, amount))
        // FIXME: broadcast ownWit

        // FIXME: this is temporal, in real world we need to give the tx to the initiator to be signed
        val userWit = wallet.sign(txDraft)

        // All wits are here, we can sign and submit
        val wits = peersWits + ownWit + userWit

        val initTx: L1Tx = wits.foldLeft(txDraft)(addWitness)

        log.whenInfoEnabled {
            log.info("Init tx: " + Transaction.deserialize(initTx.bytes).serializeToHex())
        }

        val ret = cardano.submit(initTx)

        // Put the head into multisig regime state
        ret match
            case Right(_) =>
                log.info(
                  s"Head was initialized at address: $headAddress, token name: $beaconTokenName"
                )
                headStateManager.init(headNativeScript, AddressBechL1(headAddress))

        ret
    }

    def deposit(r: DepositRequest): Either[DepositError, DepositResponse] = {

        val DepositRequest(
          txId: TxId,
          txIx: TxIx,
          deadline: BigInt,
          address: AddressBechL2,
          datum: Option[Datum],
          refundAddress: AddressBechL1,
          refundDatum: Option[Datum]
        ) = r

        // FIXME
        val Some(headNativeScript) = headStateManager.headNativeScript()

        val depositDatum = DepositDatum(
          decodeBech32AddressL2(address),
          Maybe.fromOption(datum.map(datumByteString)),
          deadline,
          decodeBech32AddressL1(refundAddress),
          Maybe.fromOption(datum.map(datumByteString))
        )

        val depositTxRecipe = DepositTxRecipe((r.txId, r.txIx), depositDatum)

        // Build a deposit transaction as a courtesy of Hydrozoa (no signature)
        val Right(depositTx, index) = depositTxBuilder.mkDepositTx(depositTxRecipe)
        val depositTxHash = txHash(depositTx)

        log.info(s"Deposit tx: ${serializeTx(depositTx)}")
        log.info(s"Deposit tx hash: $depositTxHash, deposit output index: $index")

        // TODO: Add a comment to explain how it's guarantees a deposit cannot be stolen by malicious peers
        val Right(refundTxDraft) =
            refundTxBuilder.mkPostDatedRefund(
              PostDatedRefundRecipe(DepositTx(depositTx), index),
              ownKeys._1
            )

        // Own signature
        val ownWit: TxKeyWitness = signTx(refundTxDraft.toTx, ownKeys._1)

        // ReqRefundLater
        val peersWits: Set[TxKeyWitness] = network.reqRefundLater(ReqRefundLater(depositTx, index))
        // FIXME: broadcast ownWit

        val wits = peersWits + ownWit

        val refundTx: L1Tx = wits.foldLeft(refundTxDraft.toTx)(addWitness)
        log.info(s"Refund tx: ${serializeTx(refundTx)}")

        // FIXME here we have to submit the deposit tx
        val Right(depositTxId) =
            cardano.submit(addWitness(depositTx, wallet.sign(depositTx))) // FIXME combine
        log.info(s"Deposit tx submitted: $depositTxId")

        Right(DepositResponse(refundTx, (depositTxHash, index)))
    }

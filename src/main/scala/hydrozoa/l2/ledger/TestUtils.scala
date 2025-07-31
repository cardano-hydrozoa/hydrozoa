package hydrozoa.l2.ledger

import com.bloxbean.cardano.client.transaction.spec.Transaction as BBTransaction
import hydrozoa.Wallet
import hydrozoa.infra.{addWitness, signTx}
import hydrozoa.infra.transitionary.*
import hydrozoa.node.{TestPeer, addressFromPeer}
import hydrozoa.node.TestPeer.*
import io.bullet.borer.Cbor
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.HashPurpose.KeyHash
import scalus.cardano.ledger.HashSize.{given_HashSize_Blake2b_224, given_HashSize_Blake2b_256}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.State

import scala.collection.immutable.HashSet

/** Given a set of inputs event, construct a withdrawal event attempting to withdraw all inputs with
  * the given key
  */
def l2EventWithdrawalFromInputsAndPeer(
    inputs: Set[TransactionInput],
    peer: TestPeer
): L2EventWithdrawal = {
    val txBody: TransactionBody = TransactionBody(
      inputs = inputs,
      outputs = IndexedSeq.empty,
      fee = Coin(0L)
    )

    val txUnsigned: Transaction = {
        Transaction(
          body = KeepRaw(txBody),
          witnessSet = TransactionWitnessSet.empty,
          isValid = true,
          auxiliaryData = None
        )

    }

    // N.B.: round-tripping through bloxbean because this is the only way I know how to sign right now
    // Its probably possible to extract the key and use the crypto primitives from scalus directly
    L2EventWithdrawal(signTx(peer, txUnsigned))
}

/** Creates a pubkey transaction yielding a single UTxO from a set of inputs */
def l2EventTransactionFromInputsAndPeer(
    inputs: Set[TransactionInput],
    utxoSet: Map[TransactionInput, TransactionOutput],
    inPeer: TestPeer,
    outPeer: TestPeer
): L2EventTransaction = {

    val totalVal: Value = inputs.foldLeft(Value.zero)((v, ti) => v + utxoSet(ti).value)

    val txBody: TransactionBody = TransactionBody(
      inputs = inputs,
      outputs = IndexedSeq(
        Babbage(
          address = addressFromPeer(outPeer),
          value = totalVal,
          datumOption = None,
          scriptRef = None
        )
      ).map(Sized(_)),
      fee = Coin(0L)
    )

    val txUnsigned: Transaction = {
        Transaction(
          body = KeepRaw(txBody),
          witnessSet = TransactionWitnessSet.empty,
          isValid = false,
          auxiliaryData = None
        )
    }

    L2EventTransaction(signTx(inPeer, txUnsigned))
}

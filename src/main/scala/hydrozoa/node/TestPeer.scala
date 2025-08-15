package hydrozoa.node

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import hydrozoa.infra.transitionary.toScalus
import hydrozoa.infra.{WalletModuleBloxbean, addWitness}
import hydrozoa.l2.ledger.{L2EventTransaction, L2EventWithdrawal}
import hydrozoa.node.TestPeer.account
import hydrozoa.node.state.WalletId
import hydrozoa.*
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{
    Coin,
    Hash,
    KeepRaw,
    Sized,
    TransactionBody,
    TransactionInput,
    TransactionOutput,
    TransactionWitnessSet,
    Value,
    Transaction as STransaction,
    given
}
import scalus.ledger.api.v3

import scala.collection.mutable

enum TestPeer(ix: Int) derives CanEqual:
    case Alice extends TestPeer(0)
    case Bob extends TestPeer(1)
    case Carol extends TestPeer(2)
    case Daniella extends TestPeer(3)
    case Erin extends TestPeer(4)
    case Frank extends TestPeer(5)
    case Gustavo extends TestPeer(6)
    case Hector extends TestPeer(7)
    case Isabel extends TestPeer(8)
    case Julia extends TestPeer(9)

    def compareTo(another: TestPeer): Int = this.toString.compareTo(another.toString)

object TestPeer:
    private val mnemonic: String =
        "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test sauce"

    private val accountCache: mutable.Map[TestPeer, Account] = mutable.Map.empty
        .withDefault(peer =>
            new Account(
              Network(0, 42),
              mnemonic,
              createExternalAddressDerivationPathForAccount(peer.ordinal)
            )
        )

    private val walletCache: mutable.Map[TestPeer, Wallet] = mutable.Map.empty
        .withDefault(peer =>
            Wallet(
              peer.toString,
              WalletModuleBloxbean,
              account(peer).hdKeyPair().getPublicKey,
              account(peer).hdKeyPair().getPrivateKey
            )
        )

    private val addressCache: mutable.Map[TestPeer, ShelleyAddress] =
        mutable.Map.empty.withDefault(peer =>
            ShelleyAddress(
              network = Testnet,
              payment =
                  Key(Hash(blake2b_224(ByteString.fromArray(account(peer).publicKeyBytes())))),
              delegation = Null
            )
        )

    def account(peer: TestPeer): Account = accountCache.cache(peer)

    def mkWallet(peer: TestPeer): Wallet = walletCache.cache(peer)

    def mkWalletId(peer: TestPeer): WalletId = WalletId(peer.toString)

    def address(peer: TestPeer): ShelleyAddress = addressCache.cache(peer)

extension [K, V](map: mutable.Map[K, V])
    def cache(key: K): V = map.get(key) match {
        case None =>
            val missing = map.default(key)
            map.put(key, missing)
            missing
        case Some(value) => value
    }

// TODO: refactor all of this to make it just use the scalus types.
def signTx[L <: AnyLayer](peer: TestPeer, txUnsigned: Tx[L]): Tx[L] =
    val keyWitness = TestPeer.mkWallet(peer).createTxKeyWitness(txUnsigned)
    addWitness(txUnsigned, keyWitness)

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

    val txUnsigned: TxL2 = Tx[L2](
      STransaction(
        body = KeepRaw(txBody),
        witnessSet = TransactionWitnessSet.empty,
        isValid = true,
        auxiliaryData = None
      )
    )

    // N.B.: round-tripping through bloxbean because this is the only way I know how to sign right now
    // Its probably possible to extract the key and use the crypto primitives from scalus directly
    L2EventWithdrawal(signTx(peer, txUnsigned))
}

/** Creates a pubkey transaction yielding a single UTxO from a set of inputs */
def l2EventTransactionFromInputsAndPeer(
    inputs: Set[UtxoId[L2]],
    utxoSet: Map[UtxoId[L2], Output[L2]],
    inPeer: TestPeer,
    outPeer: TestPeer
): L2EventTransaction = {

    val totalVal: Value = inputs.foldLeft(Value.zero)((v, ti) => v + utxoSet(ti).value)

    val txBody: TransactionBody = TransactionBody(
      inputs = inputs.map(_.untagged),
      outputs = IndexedSeq(
        Babbage(
          address = TestPeer.address(outPeer),
          value = totalVal,
          datumOption = None,
          scriptRef = None
        )
      ).map(b => Sized(b.asInstanceOf[TransactionOutput])),
      fee = Coin(0L)
    )

    val txUnsigned: Tx[L2] = Tx[L2](
      STransaction(
        body = KeepRaw(txBody),
        witnessSet = TransactionWitnessSet.empty,
        isValid = false,
        auxiliaryData = None
      )
    )

    L2EventTransaction(signTx(inPeer, txUnsigned))
}

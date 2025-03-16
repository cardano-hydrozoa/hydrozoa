package hydrozoa.infra

import co.nstant.in.cbor.model.{Array, ByteString, Map, UnsignedInteger}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.crypto.*
import com.bloxbean.cardano.client.crypto.bip32.{HdKeyGenerator, HdKeyPair}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.l2.ledger.state.{OrderedUtxosDiff, TxIn, unwrapTxIn, unwrapTxOut}
import hydrozoa.l2.ledger.{SimpleGenesis, SimpleTransaction, SimpleWithdrawal}
import scalus.bloxbean.Interop
import scalus.builtin.{Data, ByteString as ScalusByteString}
import scalus.ledger.api.v1
import scalus.ledger.api.v1.{TokenName, TxOut}
import scalus.prelude.AssocMap
import scalus.prelude.Maybe.Just
import scalus.prelude.Prelude.given_Eq_ByteString

import scala.jdk.CollectionConverters.*

// TODO: make an API

def txHash(tx: L1Tx): TxId = TxId(getTxHash(tx.bytes))

// TODO: generalize fot both L1 and L2
def serializeTxHex(tx: L1Tx): String = HexUtil.encodeHexString(tx.bytes)

// TODO: generalize fot both L1 and L2
def deserializeTxHex(hex: String): L1Tx = L1Tx(HexUtil.decodeHexString(hex))

// Pure function to create a transaction key witness with a HD key.
// TODO: handle exceptions
def createTxKeyWitness(tx: L1Tx, pair: HdKeyPair): TxKeyWitness = {

    // See TransactionSigner

    val txBytes = TransactionBytes(tx.bytes)
    val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
    val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
    val signature = signingProvider.signExtended(
      txnBodyHash,
      pair.getPrivateKey.getKeyData,
      pair.getPublicKey.getKeyData
    )
    TxKeyWitness(signature, pair.getPublicKey.getKeyData)
}

// Pure function to create a transaction key witness with a peer node's key.
// TODO: handle exceptions
def createTxKeyWitness(tx: L1Tx, participantKey: ParticipantSecretKey): TxKeyWitness = {

    // See TransactionSigner

    val secretKey = SecretKey.create(participantKey.bytes)
    val txBytes = TransactionBytes(tx.bytes)
    val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
    val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider

    val (signature, vKey) =
        if (secretKey.getBytes.length == 64) { // extended pvt key (most prob for regular account)
            // check for public key
            val vBytes = HdKeyGenerator.getPublicKey(secretKey.getBytes)
            val vKey = VerificationKey.create(vBytes)
            val sig = signingProvider.signExtended(txnBodyHash, secretKey.getBytes, vBytes)
            (sig, vKey)
        } else {
            val sig = signingProvider.sign(txnBodyHash, secretKey.getBytes)
            val vKey = KeyGenUtil.getPublicKeyFromPrivateKey(secretKey)
            (sig, vKey)
        }
    TxKeyWitness(signature, vKey.getBytes)
}

// Pure function to add a key witness to a transaction.
def addWitness(tx: L1Tx, wit: TxKeyWitness): L1Tx = {
    val txBytes = TransactionBytes(tx.bytes)
    val witnessSetDI = CborSerializationUtil.deserialize(txBytes.getTxWitnessBytes)
    val witnessSetMap = witnessSetDI.asInstanceOf[Map]

    val vkWitnessArrayDI = witnessSetMap.get(UnsignedInteger(0))

    val vkWitnessArray: Array =
        if (vkWitnessArrayDI != null) vkWitnessArrayDI.asInstanceOf[Array]
        else new Array

    if (vkWitnessArrayDI == null)
        witnessSetMap.put(new UnsignedInteger(0), vkWitnessArray)

    val vkeyWitness = new Array
    vkeyWitness.add(ByteString(wit.vkey))
    vkeyWitness.add(ByteString(wit.signature))

    vkWitnessArray.add(vkeyWitness)

    val txWitnessBytes = CborSerializationUtil.serialize(witnessSetMap, false)
    L1Tx(txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes)
}

/** @param tx
  * @param address
  * @return
  *   Index and ada amount (should be value).
  */
def onlyAddressOutput(tx: L1Tx, address: AddressBechL1): Option[(TxIx, BigInt)] =
    val tx_ = Transaction.deserialize(tx.bytes)
    val outputs = tx_.getBody.getOutputs.asScala
    outputs.indexWhere(output => output.getAddress == address.bech32) match
        case -1 => None
        case i  => Some((TxIx(i), BigInt.apply(outputs.apply(i).getValue.getCoin.longValue())))

def outputDatum(tx: L1Tx, index: TxIx): Data =
    val tx_ = Transaction.deserialize(tx.bytes)
    val output = tx_.getBody.getOutputs.get(index.ix.intValue)
    val datum = output.getInlineDatum
    Interop.toScalusData(datum)

def txInputsRef(tx: L1Tx): Set[(TxId, TxIx)] =
    val tx_ = Transaction.deserialize(tx.bytes)
    tx_.getBody.getInputs.asScala.map(ti => (TxId(ti.getTransactionId), TxIx(ti.getIndex))).toSet

/** @param genesis
  * @return
  *   Virtual genesis tx that spends L1 deposit utxos and produces L2 genesis utxos.
  */
def mkVirtualGenesisTx(genesis: SimpleGenesis): L1Tx =

    val virtualInputs = genesis.virtualInputs.map { input =>
        TransactionInput.builder
            .transactionId(input.id.hash)
            .index(input.ix.ix.intValue)
            .build
    }.toList

    val virtualOutputs = genesis.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    L1Tx(tx.serialize)

/** @param simpleTx
  * @return
  *   Virtual genesis tx that spends L1 deposit utxos and produces L2 genesis utxos.
  */
def mkVirtualTransactionL2(simpleTx: SimpleTransaction): L1Tx =

    val virtualInputs = simpleTx.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

    val virtualOutputs = simpleTx.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    L1Tx(tx.serialize)

def toBloxBeanTransactionOutput(output: TxOut): TransactionOutput =
    val Just(e) = AssocMap.lookup(output.value)(ScalusByteString.empty)
    val Just(coins) = AssocMap.lookup(e)(ScalusByteString.empty)
    TransactionOutput.builder
        .address(
          addressToBloxbean(AppCtx.yaciDevKit().network, output.address).getAddress
        ) // FIXME: network
        .value(Value.builder.coin(coins.bigInteger).build)
        .build

def toBloxBeanTransactionInput(input: v1.TxOutRef): TransactionInput = {
    TransactionInput
        .builder()
        .transactionId(input.id.hash.toHex)
        .index(input.idx.intValue)
        .build()
}

/** @param withdrawas
  * @return
  *   Virtual genesis tx that spends L1 deposit utxos and produces L2 genesis utxos.
  */
def mkVirtualWithdrawalTx(
    withdrawal: SimpleWithdrawal
    // , virtualOutputs: List[TxOut]
): L1Tx =

    val virtualInputs = withdrawal.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

//    val outputsL1 = virtualOutputs.map(toBloxBeanTransactionOutput)
//
    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
//        .outputs(outputsL1.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    L1Tx(tx.serialize)

//def augmentWithVirtualInputs(tx: L1Tx, virtualInputs: Set[TxIn]): L1Tx =
//    val tx_ = Transaction.deserialize(tx.bytes)
//    tx_.getBody.getInputs.addAll(
//      virtualInputs.map(v => toBloxBeanTransactionInput(unwrapTxIn(v))).toList.asJava
//    )
//    L1Tx(tx_.serialize())

def augmentWithdrawal(tx: L1Tx, settlementTx: L1Tx, taken: Int): (L1Tx, Int) =
    val tx_ = Transaction.deserialize(tx.bytes)
    val settlementTx_ = Transaction.deserialize(settlementTx.bytes)
    val n = tx_.getBody.getInputs.size
    val withdrawalOutputsL1 = settlementTx_.getBody.getOutputs.asScala.drop(taken).take(n)
    tx_.getBody.getOutputs.addAll(withdrawalOutputsL1.asJava)
    (L1Tx(tx_.serialize), n)

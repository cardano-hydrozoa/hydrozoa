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
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag}
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

def txHash[T <: MultisigTxTag, L <: AnyLevel](tx: MultisigTx[T] | Tx[L]): TxId = TxId(
  getTxHash(getAnyTxBytes(tx))
)

def serializeTxHex[T <: MultisigTxTag, L <: AnyLevel](tx: MultisigTx[T] | Tx[L]): String =
    HexUtil.encodeHexString(getAnyTxBytes(tx))

def getAnyTxBytes[L <: AnyLevel, T <: MultisigTxTag](tx: MultisigTx[T] | Tx[L]) =
    tx match
        case multisig: MultisigTx[T] => MultisigTx.toL1Tx(multisig).bytes
        case tx: Tx[L]               => tx.bytes

// TODO: generalize fot both L1 and L2
def deserializeTxHex(hex: String): TxAny = Tx[AnyLevel](HexUtil.decodeHexString(hex))

// Pure function to create a transaction key witness with a HD key.
// TODO: handle exceptions
def createTxKeyWitness[T <: MultisigTxTag](tx: MultisigTx[T], pair: HdKeyPair): TxKeyWitness = {

    // See TransactionSigner

    val txBytes = TransactionBytes(MultisigTx.toL1Tx(tx).bytes)
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
def createTxKeyWitness[T <: MultisigTxTag](
    tx: MultisigTx[T],
    participantKey: ParticipantSecretKey
): TxKeyWitness = {

    // See TransactionSigner

    val secretKey = SecretKey.create(participantKey.bytes)
    val txBytes = TransactionBytes(MultisigTx.toL1Tx(tx).bytes)
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
def addWitness[T <: MultisigTxTag](tx: MultisigTx[T], wit: TxKeyWitness): MultisigTx[T] = {
    val txBytes = TransactionBytes(MultisigTx.toL1Tx(tx).bytes)
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
    MultisigTx(TxL1(txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes))
}

/** @param tx
  * @param address
  * @return
  *   Index and ada amount (should be value).
  */
def onlyAddressOutput(tx: TxAny, address: AddressBechL1): Option[(TxIx, BigInt)] =
    val tx_ = Transaction.deserialize(tx.bytes)
    val outputs = tx_.getBody.getOutputs.asScala
    outputs.indexWhere(output => output.getAddress == address.bech32) match
        case -1 => None
        case i  => Some((TxIx(i), BigInt.apply(outputs.apply(i).getValue.getCoin.longValue())))

def outputDatum(tx: TxAny, index: TxIx): Data =
    val tx_ = Transaction.deserialize(tx.bytes)
    val output = tx_.getBody.getOutputs.get(index.ix.intValue)
    val datum = output.getInlineDatum
    Interop.toScalusData(datum)

// TODO: unused
def txInputs[L <: AnyLevel](tx: TxAny): Set[OutputRef[L]] =
    val tx_ = Transaction.deserialize(tx.bytes)
    tx_.getBody.getInputs.asScala
        .map(ti => OutputRef[L](TxId(ti.getTransactionId), TxIx(ti.getIndex)))
        .toSet

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

// ----------------------------------------------------------------------------
// Cardano L2 transactions for the simplified ledger
// ----------------------------------------------------------------------------

def mkCardanoTxForL2Genesis(genesis: SimpleGenesis): TxL2 =

    val virtualOutputs = genesis.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    TxL2(tx.serialize)

/** @param simpleTx
  * @return
  *   Virtual L2 transaction that spends L1 deposit utxos and produces L2 genesis utxos.
  */
def mkCardanoTxForL2Transaction(simpleTx: SimpleTransaction): TxL2 =

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
    TxL2(tx.serialize)

/** @param withdrawal
  * @return
  */
def mkCardanoTxForL2Withdrawal(withdrawal: SimpleWithdrawal): TxL2 =

    val virtualInputs = withdrawal.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    Tx[L2](tx.serialize)

//def augmentWithVirtualInputs[L <: AnyLevel](tx: Tx[L], virtualInputs: Set[OutputRefInt]): Tx[L] =
//    val tx_ = Transaction.deserialize(tx.bytes)
//    tx_.getBody.getInputs.addAll(
//      virtualInputs.map(v => toBloxBeanTransactionInput(unwrapTxIn(v))).toList.asJava
//    )
//    Tx[L](tx_.serialize())

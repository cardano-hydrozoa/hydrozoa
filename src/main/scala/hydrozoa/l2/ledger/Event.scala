package hydrozoa.l2.ledger

import hydrozoa.*
import hydrozoa.infra.transitionary.toScalusLedger
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.state.DepositDatum.derived$FromData
import io.bullet.borer.Cbor
import scalus.builtin.Builtins.blake2b_256
import scalus.builtin.Data.fromData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage

// A sum type for ledger events
sealed trait L2Event:
    def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash]

final case class L2EventTransaction(transaction: TxL2) extends L2Event {
    override def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash] = transaction.id
    def volume: Long = transaction.body.value.outputs.map(sto => sto.value.value.coin.value).sum
}
final case class L2EventWithdrawal(transaction: TxL2) extends L2Event {
    override def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash] = transaction.id
}

/** A genesis event absorbs a number of transaction inputs from L1 and produces corresponding L2
  * outputs. The TxId of a Genesis Event comes from sorting the TxIds of the absorbed UTxOs,
  * encoding them to Cbor, concatenating, and taking the blake2b_256 hash.
  */
final case class L2EventGenesis(utxosL1: Seq[(UtxoIdL1, OutputL1)]) extends L2Event {
    require(utxosL1.nonEmpty, "L2EventGenesis must consume at least one L1 deposit")

    /** The list of UTxOs that should appear on L2 corresponding to this genesis event. Malformed
      * deposit UTxOs (non-Babbage outputs or with invalid datums) are skipped.
      */
    // FIXME: check if this is really the desired behavior. Should we do something else if we have a malformed
    // deposit output?
    val resolvedL2UTxOs: Seq[(UtxoIdL2, OutputL2)] = {
        val mbL2Outs: Seq[Option[(UtxoIdL2, OutputL2)]] = {
            utxosL1.zipWithIndex.map((utxo, idx) => {
                val l2TxIn = TransactionInput(transactionId = getEventId, index = idx)
                if !utxo._2.isInstanceOf[Babbage] then None
                else {
                    val babbageTxOut = utxo._2.asInstanceOf[Babbage]
                    babbageTxOut.datumOption match {
                        case Some(Inline(datum)) =>
                            // FIXME, seriously: this is chaotic. We previously were using `ByteString` to represent datums,
                            // and because of that, they got serialized as Bytestrings. Thus, the datum on the deposit
                            // utxo is serialized _as a bytestring that contains the cbor for the actual datum_.
                            val dd: DepositDatum = fromData(datum)
                            Some(
                              UtxoId[L2](l2TxIn),
                              Output[L2](
                                Babbage(
                                  address = dd.address.toScalusLedger,
                                  value = Value(babbageTxOut.value.coin),
                                  datumOption = dd.datum.asScala.map(Inline(_)),
                                  scriptRef = None
                                )
                              )
                            )

                        case _ => None
                    }
                }
            })

        }
        mbL2Outs.filter(_.isDefined).map(_.get)
    }

    override def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash] = Hash(
      blake2b_256(
        ByteString.fromArray(
          // I know this is an insane way to do it, but transaction input apparently doesn't have an ordering instance
          // yet
          utxosL1
              .map((ti, to) => ti.transactionId.toHex ++ ti.index.toString)
              .sorted
              .flatMap(ti => Cbor.encode(ti).toByteArray)
              .toArray
        )
      )
    )
    def volume: Long = utxosL1.map((ti, to) => to.value.coin.value).sum

}

/** Tags used in blocks */
sealed trait L2EventLabel derives CanEqual

case object L2EventGenesisLabel extends L2EventLabel derives CanEqual
case object L2EventTransactionLabel extends L2EventLabel derives CanEqual
case object L2EventWithdrawalLabel extends L2EventLabel derives CanEqual

def l2EventLabel(e: L2Event): L2EventLabel =
    e match
        case _: L2EventGenesis     => L2EventGenesisLabel
        case _: L2EventTransaction => L2EventTransactionLabel
        case _: L2EventWithdrawal  => L2EventWithdrawalLabel

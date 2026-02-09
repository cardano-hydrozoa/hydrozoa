package hydrozoa.multisig.ledger.virtual

import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import scala.collection.immutable.Queue
import scalus.builtin.Data
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Hash as _, *}
import scalus.prelude.Option as SOption

// A sum type for ledger events
sealed trait L2Event

// TODO: Run L2 conformance during parsing?
final case class L2EventTransaction(transaction: Transaction) extends L2Event {
    def volume: Long = transaction.body.value.outputs.map(sto => sto.value.value.coin.value).sum
}

// TODO: Rename to L2Genesis
// TODO: Fix to work with the new way that virtual utxos are created in deposit transactions.
object L2EventGenesis:
    enum L2EventGenesisError:
        case EmptyInputs

final case class L2EventGenesis(
    // We allow  this to be empty so that we can do the "push the fallback forward" tx
    genesisObligations: Queue[GenesisObligation],
    // blake2b_256(treasuryTokenName.bytestring ++ nextBlockVersion)
    // TODO: Type this better? It shouldn't really be a TransactionHash, because it's
    // preimage is not a [[Transaction]]
    genesisId: TransactionHash
) extends L2Event {
    val asUtxos: Utxos = {
        Map.from(
          genesisObligations.toList.zipWithIndex.map(x =>
              (TransactionInput(genesisId, x._2), x._1.toBabbage)
          )
        )
    }
}

/** A genesis obligation is the boundary between the L1 and L2 ledgers. It contains the well-formed
  * fields of L2-conformant UTxOs.
  */
case class GenesisObligation(
    l2OutputPaymentAddress: ShelleyPaymentPart,
    l2OutputNetwork: Network,
    l2OutputDatum: SOption[Data],
    l2OutputValue: Coin,
    l2OutputRefScript: Option[Script.Native | Script.PlutusV3]
) {
    def toBabbage: Babbage =
        Babbage(
          address = ShelleyAddress(
            network = l2OutputNetwork,
            payment = l2OutputPaymentAddress,
            delegation = ShelleyDelegationPart.Null
          ),
          value = Value(l2OutputValue),
          datumOption = l2OutputDatum match {
              case SOption.Some(data) => Some(Inline(data))
              case SOption.None       => None
          },
          scriptRef = l2OutputRefScript.map(ScriptRef(_))
        )
}

object GenesisObligation {
    import DepositRefundTxSeq.Parse.ParseErrorOr
    import DepositRefundTxSeq.Parse.Error.*

    def fromTransactionOutput(to: TransactionOutput): ParseErrorOr[GenesisObligation] =
        to match {
            case o: TransactionOutput.Babbage =>
                for {
                    shelleyAddress: ShelleyAddress <- o.address match {
                        case sa: ShelleyAddress if sa.delegation == ShelleyDelegationPart.Null =>
                            Right(sa)
                        case _ => Left(VirtualOutputNotShelleyAddress(o))
                    }
                    datum <- o.datumOption match {
                        case None            => Right(SOption.None)
                        case Some(i: Inline) => Right(SOption.Some(i.data))
                        case Some(_)         => Left(VirtualOutputDatumNotInline(o))
                    }
                    coin <-
                        if o.value.assets.isEmpty
                        then Right(o.value.coin)
                        else Left(VirtualOutputMultiAssetNotEmpty(o))
                    refScript: Option[Native | Script.PlutusV3] <- o.scriptRef match {
                        case None                                => Right(None)
                        case Some(ScriptRef(s: Script.PlutusV3)) => Right(Some(s))
                        case Some(ScriptRef(s: Native))          => Right(Some(s))
                        case Some(_) => Left(VirtualOutputRefScriptInvalid(o))
                    }
                } yield GenesisObligation(
                  l2OutputPaymentAddress = shelleyAddress.payment,
                  l2OutputNetwork = shelleyAddress.network,
                  l2OutputDatum = datum,
                  l2OutputValue = coin,
                  l2OutputRefScript = refScript,
                )
            case o: TransactionOutput.Shelley => Left(NonBabbageVirtualOutput(o))
        }
}

package hydrozoa.multisig.ledger.virtual.tx

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import io.bullet.borer.Cbor
import scala.collection.immutable.{Queue, TreeMap}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, Hash32, KeepRaw, Script, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Value}
import scalus.cardano.onchain.plutus.prelude.Option as SOption
import scalus.uplc.builtin.{ByteString, Data, platform}

final case class L2Genesis(
    // We allow  this to be empty so that we can do the "push the fallback forward" tx
    // TODO: do we need Queue here though?
    genesisObligations: Queue[GenesisObligation],
    // blake2b_256(treasuryTokenName.bytestring ++ nextBlockVersion)
    // TODO: Type this better? It shouldn't really be a TransactionHash, because it's
    // preimage is not a [[Transaction]]
    genesisId: TransactionHash
) {
    val asUtxos: TreeMap[TransactionInput, KeepRaw[TransactionOutput]] = {
        TreeMap.from(
          genesisObligations.toList.zipWithIndex.map(x =>
              (TransactionInput(genesisId, x._2), KeepRaw(x._1.toTransactionOutput))
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
    def toTransactionOutput: TransactionOutput =
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

    // TODO: Shall we use dedicated types instead?
    import DepositRefundTxSeq.Parse.Error.*
    import DepositRefundTxSeq.Parse.ParseErrorOr

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

    def serialize(obligations: NonEmptyList[GenesisObligation]): Array[Byte] =
        Cbor
            .encode(
              obligations.toList.map(_.toTransactionOutput.asInstanceOf[TransactionOutput])
            )
            .toByteArray

    def hash(obligations: NonEmptyList[GenesisObligation]): Hash32 =
        Hash[Blake2b_256, Any](
          platform.blake2b_256(ByteString.unsafeFromArray(serialize(obligations)))
        )
}

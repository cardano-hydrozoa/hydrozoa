package hydrozoa.multisig.ledger.virtual.tx

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.DepositTuple
import io.bullet.borer.derivation.MapBasedCodecs.derived
import io.bullet.borer.{Cbor, Decoder, Encoder, Writer}
import scala.collection.immutable.{Queue, TreeMap}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, KeepRaw, Script, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Value}
import scalus.cardano.onchain.plutus.prelude.Option as SOption
import scalus.uplc.builtin.{ByteString, Data, platform}

final case class L2Genesis(
    // We allow  this to be empty so that we can do the "push the fallback forward" tx
    // TODO: do we need Queue here though?
    genesisObligations: Queue[GenesisObligation],
    // This is either:
    // - The blake2b_256 hash of the seed utxo TransactionInput (for the initial genesis)
    // - The blake2b_256 hash of the deposit utxo TransactionInput (for deposits)
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

given Encoder[L2Genesis] = Encoder.derived
given l2GenesisDecoder: Decoder[L2Genesis] =
    Decoder.derived[L2Genesis]

object L2Genesis {
    def mkGenesisId(ti: TransactionInput): TransactionHash =
        TransactionHash.fromByteString(
          platform.blake2b_256(ByteString.fromArray(Cbor.encode(ti).toByteArray))
        )

    /** Warning: this is partial, but I'm keeping with the conventions of the CBOR decoder.
      */
    def fromDepositTuple(
        depositTuple: DepositTuple,
    ): L2Genesis = {
        val genesisObligations = Cbor
            .decode(depositTuple.l2Payload)
            .to[Queue[GenesisObligation]]
            .value
        val genesisId: TransactionHash =
            mkGenesisId(depositTuple.depositTransactionInput)
        L2Genesis(genesisObligations, genesisId)
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
given Encoder[GenesisObligation] with {
    override def write(w: Writer, value: GenesisObligation): Writer =
        summon[Encoder[TransactionOutput]].write(w, value.toTransactionOutput)
}
given genesisObligationDecoder: Decoder[GenesisObligation] =
    summon[Decoder[TransactionOutput]].mapEither(to => GenesisObligation.fromTransactionOutput(to))

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

    // Recall: users need to submit a NonEmptyList of genesis obligations as the L2 payload, but
    // we also need to be able to serialize an empty list for the "push forward" deposit
    def serialize(gos: NonEmptyList[GenesisObligation]): Array[Byte] =
        Cbor.encode(Queue.from(gos.toList)).toByteArray

}

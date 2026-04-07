package hydrozoa.multisig.ledger.eutxol2.tx

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.l2.L2LedgerCommand
import io.bullet.borer.derivation.MapBasedCodecs.derived
import io.bullet.borer.{Cbor, Decoder, Encoder, Writer}
import scala.collection.immutable.{Queue, TreeMap}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Blake2b_256, Hash, KeepRaw, Script, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Value}
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

    /** A hash of the deposit utxo transaction input
      * @param ti
      * @return
      */
    def mkGenesisId(ti: TransactionInput): TransactionHash =
        TransactionHash.fromByteString(
          platform.blake2b_256(ByteString.fromArray(Cbor.encode(ti).toByteArray))
        )

    /** Warning: this is partial, but I'm keeping with the conventions of the CBOR decoder.
      */
    def fromDepositEventRegistration(
        req: L2LedgerCommand.RegisterDeposit,
    ): L2Genesis = {
        val genesisObligations = Cbor
            .decode(req.l2Payload.bytes)
            .to[Queue[GenesisObligation]]
            .value
        val genesisId: TransactionHash =
            mkGenesisId(req.depositId)
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
    l2OutputValue: Value,
    l2OutputRefScript: Option[Script.Native | Script.PlutusV3]
) {
    def toTransactionOutput: TransactionOutput =
        Babbage(
          address = ShelleyAddress(
            network = l2OutputNetwork,
            payment = l2OutputPaymentAddress,
            delegation = ShelleyDelegationPart.Null
          ),
          value = l2OutputValue,
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
    enum Error extends Throwable:
        case L2OutputNotShelleyAddress(babbage: Babbage)
        case L2OutputDatumNotInline(babbage: Babbage)
        case L2OutputRefScriptInvalid(babbage: Babbage)
        case NonBabbageL2Output(shelley: TransactionOutput.Shelley)

    // TODO: Shall we use dedicated types instead?
    import DepositRefundTxSeq.Parse.Error.*

    def fromTransactionOutput(to: TransactionOutput): Either[Error, GenesisObligation] = {
        import Error.*
        to match {
            case o: TransactionOutput.Babbage =>
                for {
                    shelleyAddress: ShelleyAddress <- o.address match {
                        case sa: ShelleyAddress if sa.delegation == ShelleyDelegationPart.Null =>
                            Right(sa)
                        case _ => Left(L2OutputNotShelleyAddress(o))
                    }
                    datum <- o.datumOption match {
                        case None            => Right(SOption.None)
                        case Some(i: Inline) => Right(SOption.Some(i.data))
                        case Some(_)         => Left(L2OutputDatumNotInline(o))
                    }
                    refScript: Option[Native | Script.PlutusV3] <- o.scriptRef match {
                        case None                                => Right(None)
                        case Some(ScriptRef(s: Script.PlutusV3)) => Right(Some(s))
                        case Some(ScriptRef(s: Native))          => Right(Some(s))
                        case Some(_) => Left(L2OutputRefScriptInvalid(o))
                    }
                } yield GenesisObligation(
                  l2OutputPaymentAddress = shelleyAddress.payment,
                  l2OutputNetwork = shelleyAddress.network,
                  l2OutputDatum = datum,
                  l2OutputValue = o.value,
                  l2OutputRefScript = refScript,
                )
            case o: TransactionOutput.Shelley => Left(NonBabbageL2Output(o))
        }
    }

    // Recall: users need to submit a NonEmptyList of genesis obligations as the L2 payload, but
    // we also need to be able to serialize an empty list for the "push forward" deposit
    def serialize(gos: NonEmptyList[GenesisObligation]): ByteString =
        ByteString.fromArray(Cbor.encode(Queue.from(gos.toList)).toByteArray)

}

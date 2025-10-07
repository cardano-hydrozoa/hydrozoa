package hydrozoa.rulebased.ledger.dapp.utxo

import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum
import scalus.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, TransactionInput, TransactionOutput, Value}

// TODO: Make opaque
final case class RuleBasedTreasuryUtxo(
    beaconTokenName: AssetName,
    txId: TransactionInput,
    addr: ShelleyAddress,
    datum: RuleBasedTreasuryDatum,
    value: Value
) {
    def toUtxo: (TransactionInput, Babbage) =
        (
          txId,
          Babbage(
            address = addr,
            value = value,
            datumOption = Some(Inline(datum.toData)),
            scriptRef = None
          )
        )
}

object RuleBasedTreasuryUtxo {

    /** This is all placeholder stuff: */

    private type H32 = ByteString

    private type KzgCommit = ByteString

    private type VersionMajor = BigInt

//def mkTreasuryDatumUnresolved(
//    headMp: PolicyId,
//    disputeId: TokenName,
//    peers: List[VerificationKeyBytes],
//    deadlineVoting: HPosixTime,
//    versionMajor: BigInt,
//    params: Unit // TODO: L2ConsensusParamsH32
//): TreasuryDatum =
//    UnresolvedDatum(
//      headMp = headMp,
//      disputeId = disputeId,
//      peers = peers.map(_.bytes),
//      peersN = peers.length,
//      deadlineVoting = deadlineVoting,
//      versionMajor = versionMajor,
//      params = ???,
//      // TODO: magic number, arguably should be a parameter
//      setup = TrustedSetup.takeSrsG2(10).map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
//    ) |> Unresolved.apply

    //    def fromUtxo(utxo : (TransactionInput, TransactionOutput)) : Option[TreasuryUtxo] = {
//        val tuxo = for {
//           datum <- Try(fromData[TreasuryUtxo.Datum](utxo._2.asInstanceOf[Babbage].datumOption.get.asInstanceOf[Inline].data))
//           va
//        } yield ???
//
//        tuxo match {
//            case Success(v) => Some(v)
//            case Failure(e) => None
//        }
//    }
}

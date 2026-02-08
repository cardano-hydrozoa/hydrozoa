package hydrozoa.rulebased.ledger.dapp.utxo

import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum
import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, TransactionInput, TransactionOutput, Utxo, Value}

// TODO: Make opaque
final case class RuleBasedTreasuryUtxo(
    treasuryTokenName: AssetName,
    utxoId: TransactionInput,
    address: ShelleyAddress,
    datum: RuleBasedTreasuryDatum,
    value: Value
) {
    val output: Babbage =
        Babbage(
          address = address,
          value = value,
          datumOption = Some(Inline(datum.toData)),
          scriptRef = None
        )

    val asTuple: (TransactionInput, Babbage) =
        (
          utxoId,
          output
        )

    val asUtxo: Utxo =
        Utxo(utxoId, output)
}

object RuleBasedTreasuryUtxo {
    trait Produced {
        def treasuryProduced: RuleBasedTreasuryUtxo
    }

    trait Spent {
        def treasurySpent: RuleBasedTreasuryUtxo
    }
    // def mkTreasuryDatumUnresolved(
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

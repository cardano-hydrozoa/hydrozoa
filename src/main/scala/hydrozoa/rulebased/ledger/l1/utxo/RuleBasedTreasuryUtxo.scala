package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{RuleBasedTreasuryDatum, RuleBasedTreasuryDatumOnchain}
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryOutput.{Config, *}
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}
import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, MultiAsset, PolicyId, Slot, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptAttached
import scalus.cardano.txbuilder.ThreeArgumentPlutusScriptWitness
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ReferenceOutput, Send, Spend}
import scalus.uplc.builtin.Data

final case class RuleBasedTreasuryUtxo(
    utxoId: TransactionInput,
    treasuryOutput: RuleBasedTreasuryOutput
) {

    def toUtxo(using config: Config): Utxo =
        Utxo(utxoId, treasuryOutput.toOutput)

    // Other alternative is to pass a type variable to RuleBasedTreasuryUtxo indicating the datum type,
    // but we'd still have to parse at some point due to type erasure. But we could parse at the boundary instead.
    def parseVotingDeadline(using config: Config): Either[ParseError, Slot] =
        treasuryOutput.datum match {
            case RuleBasedTreasuryDatum.Unresolved(deadlineVoting, _, _) =>
                Try(Slot(config.slotConfig.timeToSlot(deadlineVoting.toLong))).toEither.left
                    .map(TreasuryDatumContainsInvalidDeadline(_))
            case _ => Left(TreasuryDatumResolved)
        }

    def referenceOutput(using config: Config): ReferenceOutput = ReferenceOutput(toUtxo)

    def spendAttached(redeemer: TreasuryRedeemer)(using config: Config) = Spend(
      toUtxo,
      ThreeArgumentPlutusScriptWitness(
        PlutusScriptAttached,
        redeemer.toData,
        DatumInlined,
        Set.empty
      )
    )
}

object RuleBasedTreasuryUtxo {

    def parse(utxo: Utxo)(using config: Config): Either[ParseError, RuleBasedTreasuryUtxo] = {

        for {
            output <- RuleBasedTreasuryOutput(utxo.output)
        } yield RuleBasedTreasuryUtxo(
          utxoId = utxo.input,
          treasuryOutput = output
        )
    }

    trait Produced {
        def treasuryProduced: RuleBasedTreasuryUtxo
    }

    trait Spent {
        def treasurySpent: RuleBasedTreasuryUtxo
    }
}

// TODO: this class could further decompose the value into "Vote tokens" and an implicit "Treasury Token".
//   The primary benefit to doing so would be encoding the expected invariants at the type level, which may make
//   model checking or formalization easier. It also gives us the ability to add a
final case class RuleBasedTreasuryOutput(datum: RuleBasedTreasuryDatum, value: Value) {

    def toOutput(using config: RuleBasedTreasuryOutput.Config): Babbage =
        Babbage(
          address = config.ruleBasedTreasuryAddress,
          value = value,
          datumOption = Some(Inline(datum.toOnchain.toData)),
          scriptRef = None
        )

    def send(using config: RuleBasedTreasuryOutput.Config): Send = Send(toOutput)

    def headTokens(using config: RuleBasedTreasuryOutput.Config): MultiAsset = {
        val inner =
            value.assets.assets(config.headMultisigScript.policyId)
        val outer: SortedMap[PolicyId, SortedMap[AssetName, Long]] = SortedMap(
          (config.headMultisigScript.policyId, inner)
        )
        MultiAsset(outer)
    }

    def burnHeadTokens(using config: RuleBasedTreasuryOutput.Config): List[Mint] = {
        val policyId = config.headMultisigScript.policyId
        val inner = headTokens.assets(policyId)
        inner.toList.map((assetName, amount) =>
            Mint(
              scriptHash = policyId,
              assetName = assetName,
              amount = -amount,
              witness = config.headMultisigScript.witnessValue
            )
        )
    }
}

object RuleBasedTreasuryOutput {
    type Config = CardanoNetwork.Section & HeadPeers.Section & HasTokenNames

    def apply(
        output: TransactionOutput
    )(using config: Config): Either[ParseError, RuleBasedTreasuryOutput] =
        for {
            d1 <- output.datumOption.toRight(TreasuryDatumMissing(output))

            d2 <- d1 match {
                case i: Inline => Right(i)
                case _         => Left(TreasuryDatumNotInline(output))
            }

            datum <- Try(fromData[RuleBasedTreasuryDatumOnchain](d2.data)) match {
                case Success(d) if d.toOffchain.nonEmpty => Right(d.toOffchain.get)
                case Failure(e) => Left(TreasuryDatumDeserializationError(output, e))
            }

            address <- output.address match {
                case sa: ShelleyAddress if sa == config.ruleBasedTreasuryAddress => Right(sa)
                case _ => Left(TreasuryAtWrongAddress(output))
            }

            value = output.value
            _ <- value.assets.assets.get(config.headMultisigScript.policyId) match {
                case None => Left(WrongTreasuryValue(value))
                case Some(innerMap) =>
                    innerMap.get(config.headTokenNames.treasuryTokenName) match {
                        case None               => Left(WrongTreasuryValue(value))
                        case Some(v) if v != 1L => Left(WrongTreasuryValue(value))
                        case Some(_)            => Right(RuleBasedTreasuryOutput(datum, value))
                    }
            }
        } yield RuleBasedTreasuryOutput(
          datum = datum,
          value = value
        )

    sealed trait ParseError extends Throwable {
        override def toString: String = getMessage

        override def getMessage: String
    }

    case class TreasuryDatumContainsInvalidDeadline(wrapped: Throwable) extends ParseError {
        override def getMessage: String =
            "Could not convert voting deadline. "
                ++ "Wrapped message: ${t.wrapped.getMessage}"
    }

    case object TreasuryDatumResolved extends ParseError {
        override def getMessage: String =
            "Needed an unresolved treasury datum, but found a resolved datum."
    }

    case class TreasuryDatumMissing(output: TransactionOutput) extends ParseError {
        override def getMessage: String =
            s"Treasury datum is missing for output ${output}"
    }

    case class TreasuryDatumNotInline(output: TransactionOutput) extends ParseError {
        override def getMessage: String =
            s"Treasury datum is not inline for output: $output"
    }

    case class TreasuryDatumDeserializationError(output: TransactionOutput, e: Throwable)
        extends ParseError {
        override def getMessage: String =
            s"Failed to deserialize treasury datum for output ${output}. Error: ${e.getMessage}"
    }

    case class TreasuryAtWrongAddress(output: TransactionOutput)(using config: Config)
        extends ParseError {
        override def getMessage: String =
            s"Treasury address is not at the correct address. Expected ${config.ruleBasedTreasuryAddress}, " +
                s"but the utxo was: $output"
    }

    case class WrongTreasuryValue(value: Value)(using config: Config) extends ParseError {
        override def getMessage
            : String = "A RuleBasedTreauryOutput for this configuration needs exactly 1 Treasury Token with policy id" +
            s" ${config.headMultisigScript.policyId} and asset name ${config.headTokenNames.treasuryTokenName}, but we found $value"
    }

}

package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.head.HeadConfig
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedRegimeValidator.RegimeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedRegimeValidator.given
import hydrozoa.rulebased.ledger.l1.state.RegimeState.RuleBasedRegimeDatum
import hydrozoa.rulebased.ledger.l1.state.RegimeState.given
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptAttached
import scalus.cardano.txbuilder.ThreeArgumentPlutusScriptWitness
import scalus.cardano.txbuilder.TransactionBuilderStep.{ReferenceOutput, Send, Spend}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{fromData, toData}

/** The rule-based regime utxo: the HRWT beacon plus the immutable head-identity datum, produced by
  * the FallbackTx at the [[hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedRegimeValidator]]
  * address. Consumed only as a reference input during the rule-based regime; spent (and its beacon
  * burned) by the DeinitTx.
  *
  * It sits at its own validator rather than at the head multisig address — where the multisig
  * regime utxo carrying `headParamsHash` lives — so that a sweep of that address cannot consume it:
  * without the regime utxo the rule-based treasury can neither resolve nor evacuate.
  */
final case class RuleBasedRegimeUtxo(input: TransactionInput) {

    def toUtxo(using config: RuleBasedRegimeOutput.Config): Utxo =
        Utxo(input, RuleBasedRegimeOutput.toOutput)

    def referenceOutput(using config: RuleBasedRegimeOutput.Config): ReferenceOutput =
        ReferenceOutput(toUtxo)

    /** Spends the regime utxo under its own validator, which accepts only a tx that burns both the
      * HRWT this utxo holds and the treasury beacon. The script comes from the deployed reference
      * utxo, so the spending tx must also carry `config.referenceRegime`.
      */
    def spend(using config: RuleBasedRegimeOutput.Config): Spend = Spend(
      toUtxo,
      ThreeArgumentPlutusScriptWitness(
        PlutusScriptAttached,
        RegimeRedeemer.Deinit.toData,
        DatumInlined
      )
    )
}

object RuleBasedRegimeUtxo {

    def parse(utxo: Utxo)(using
        config: RuleBasedRegimeOutput.Config
    ): Either[RuleBasedRegimeOutput.ParseError, RuleBasedRegimeUtxo] =
        RuleBasedRegimeOutput.validate(utxo.output).map(_ => RuleBasedRegimeUtxo(utxo.input))

    /** If some tx extends this, it means that tx is producing it. */
    trait Produced {
        def regimeUtxoProduced: RuleBasedRegimeUtxo
    }

    /** If some tx extends this, it means that tx is spending it. */
    trait Spent {
        def regimeUtxoSpent: RuleBasedRegimeUtxo
    }
}

case object RuleBasedRegimeOutput {
    type Config = HeadConfig.Bootstrap.Section

    /** The immutable head-identity datum, derived from config exactly as the on-chain consumers
      * expect it.
      */
    def datum(using config: Config): RuleBasedRegimeDatum = RuleBasedRegimeDatum(
      disputeId = config.headTokenNames.voteTokenName.bytes,
      headPeers = PList.from(config.headPeerVKeys.toList),
      headPeersN = BigInt(config.nHeadPeers.toInt),
      coilPeers = PList.from(config.coilPeerVKeys),
      coilQuorum = BigInt(config.coilQuorum),
      setupG2Ladder = {
          val anchor = config.setupLadderAnchor
          TxOutRef(
            TxId(ByteString.fromArray(anchor.transactionId.bytes)),
            BigInt(anchor.index)
          )
      }
    )

    def toOutput(using config: Config): Babbage = Babbage(
      address = config.ruleBasedRegimeAddress,
      value = Value(config.collectiveContingency.minAdaForRegime) +
          Value.asset(
            config.headMultisigScript.policyId,
            config.headTokenNames.regimeWitnessTokenName,
            1L
          ),
      datumOption = Some(Inline(datum.toData)),
      scriptRef = None
    )

    def send(using config: Config): Send = Send(toOutput)

    /** Checks that `output` is this head's regime output: right address, exactly one HRWT beacon
      * under the head multisig policy, and a datum matching the config-derived head identity.
      */
    def validate(output: TransactionOutput)(using config: Config): Either[ParseError, Unit] =
        for {
            _ <- output.address match {
                case sa: ShelleyAddress if sa == config.ruleBasedRegimeAddress => Right(())
                case _ => Left(ParseError.RegimeAtWrongAddress(output))
            }

            _ <- output.value.assets.assets
                .get(config.headMultisigScript.policyId)
                .flatMap(_.get(config.headTokenNames.regimeWitnessTokenName)) match {
                case Some(1L) => Right(())
                case _        => Left(ParseError.RegimeBeaconMissing(output))
            }

            d1 <- output.datumOption.toRight(ParseError.RegimeDatumMissing(output))
            d2 <- d1 match {
                case i: Inline => Right(i)
                case _         => Left(ParseError.RegimeDatumMissing(output))
            }
            _ <- Try(fromData[RuleBasedRegimeDatum](d2.data)) match {
                case Success(d) if d == datum => Right(())
                case Success(_)               => Left(ParseError.RegimeDatumMismatch(output))
                case Failure(e)               => Left(ParseError.RegimeDatumUnparseable(output, e))
            }
        } yield ()

    enum ParseError extends RuntimeException:
        case RegimeAtWrongAddress(output: TransactionOutput)
        case RegimeBeaconMissing(output: TransactionOutput)
        case RegimeDatumMissing(output: TransactionOutput)
        case RegimeDatumMismatch(output: TransactionOutput)
        case RegimeDatumUnparseable(output: TransactionOutput, cause: Throwable)

        override def getMessage: String = this match
            case RegimeAtWrongAddress(output) =>
                s"Regime utxo is not at the rule-based regime script address: $output"
            case RegimeBeaconMissing(output) =>
                s"Regime utxo does not hold exactly one HRWT beacon token: $output"
            case RegimeDatumMissing(output) =>
                s"Regime utxo datum is missing or not inline: $output"
            case RegimeDatumMismatch(output) =>
                s"Regime utxo datum does not match this head's configuration: $output"
            case RegimeDatumUnparseable(output, cause) =>
                s"Failed to parse regime utxo datum for output $output: ${cause.getMessage}"
}

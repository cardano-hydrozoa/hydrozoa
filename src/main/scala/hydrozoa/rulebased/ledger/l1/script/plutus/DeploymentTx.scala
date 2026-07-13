package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.data.NonEmptyList
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.lib.cardano.scalus.contextualscalus.Change
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{build, finalizeContext}
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx.Validators.nonSigningValidators
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, TxFamily}
import monocle.{Focus, Lens}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Script, ScriptRef, Timelock, Transaction, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.uplc.builtin.Data

/** Deploy tx locks each payload of [[DeploymentTxOps.Build.payloads]] into its own utxo at the
  * unspendable burn address, at output indices 0 .. n-1 (change is the last output). A payload
  * carries a reference script and/or an inline datum (e.g. a rung of the G2 setup ladder) — a
  * single utxo may hold both.
  *
  * @param deployedUtxos
  *   utxo ids where the payloads are located, in payload order
  */
final case class DeploymentTx(
    deployedUtxos: NonEmptyList[TransactionInput],
    override val tx: Transaction,
    override val txLens: Lens[DeploymentTx, Transaction] = Focus[DeploymentTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends EnrichedTx[DeploymentTx] {}

object DeploymentTx {
    given TxFamily[DeploymentTx] = TxFamily.of("DeploymentTx")
    export DeploymentTxOps.{Build, Config, DeployedPayload, mkBurnAddress}
}

private object DeploymentTxOps {

    type Config = CardanoNetwork.Section

    /** A deployed utxo's contents: an optional reference script and/or an optional inline datum. A
      * single utxo may carry both.
      */
    final case class DeployedPayload(
        scriptPayload: Option[ScriptRef],
        dataPayload: Option[Data]
    )

    object DeployedPayload {
        def script(ref: ScriptRef): DeployedPayload = DeployedPayload(Some(ref), None)
        def data(datum: Data): DeployedPayload = DeployedPayload(None, Some(datum))
    }

    enum Error extends Throwable:
        case SomeError

        override def toString: String = this match
            case SomeError => "SomeError"

        override def getMessage: String = this match
            case SomeError => "An error occurred during deployment"

    /** @param utxosToSpend
      *   utxos to pay the tx fees and storage fees for the payloads deployed AKA min ADA, should be
      *   key-locked. The change is sent back to the address of the first utxo.
      * @param payloads
      *   the payloads we are deploying, one utxo each
      */
    final case class Build(
        utxosToSpend: NonEmptyList[Utxo],
        payloads: NonEmptyList[DeployedPayload]
    ) {

        def result(using config: Config): Either[SomeBuildError | Error, DeploymentTx] = {

            val burnAddress = mkBurnAddress(config.network)

            val payloadOutputs = payloads.map(payload =>
                TransactionOutput(
                  address = burnAddress,
                  value = Value.zero,
                  datumOption = payload.dataPayload.map(Inline(_)),
                  scriptRef = payload.scriptPayload
                ).ensureMinAda(config)
            )

            val changeOutput = Babbage(
              address = utxosToSpend.head.output.address,
              value = Value.combine(utxosToSpend.toList.map(_.output.value))
                  - Value.combine(payloadOutputs.toList.map(_.value))
            )

            for {
                context <- build(
                  utxosToSpend.toList.map(u => Spend(u))
                      ++ payloadOutputs.toList.map(Send(_))
                      :+ Send(changeOutput)
                )

                finalized <- context.finalizeContext(
                  diffHandler = Change.changeOutputDiffHandler(payloads.size.toInt),
                  validators = nonSigningValidators
                )
            } yield DeploymentTx(
              deployedUtxos = payloads.zipWithIndex.map((_, i) =>
                  TransactionInput(finalized.transaction.id, i)
              ),
              tx = finalized.transaction,
              resolvedUtxos = finalized.resolvedUtxos
            )
        }

    }

    // According to https://cardano-tools.io/burn-address
    // This seems to be a bit superfluous - why do we need AllOf, why just not
    // Script.Native(Timelock.TimeExpire(0L))?
    // Anyways, AllOf won't hurt.
    private val burnScript: Script.Native =
        Script.Native(Timelock.AllOf(IndexedSeq(Timelock.TimeExpire(0L))))

    def mkBurnAddress(network: Network): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(burnScript.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )
}

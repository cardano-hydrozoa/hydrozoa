package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.data.NonEmptyList
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.lib.cardano.scalus.contextualscalus.Change
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{build, finalizeContext}
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import monocle.{Focus, Lens}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{Script, ScriptHash, ScriptRef, Timelock, Transaction, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}

/** Deploy tx deploys one script [[scriptDeployed]] into [[refScriptUtxo]] at a time. It uses
  * so-called burn address to lock the utxo so it cannot be spent.
  *
  * @param scriptDeployed
  *   the hash of the script deployed
  * @param refScriptUtxo
  *   utxo id where the script is located
  */
final case class DeploymentTx(
    scriptDeployed: ScriptHash,
    // TODO: we may split up [[ScriptReferenceUtxos]] tp make it more specific
    refScriptUtxo: TransactionInput,
    override val tx: Transaction,
    override val txLens: Lens[DeploymentTx, Transaction] = Focus[DeploymentTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[DeploymentTx] {
    override def transactionFamily: String = "DeploymentTx"
}

object DeploymentTx {
    export DeploymentTxOps.Config
}

private object DeploymentTxOps {

    type Config = CardanoNetwork.Section

    enum Error extends Throwable:
        case SomeError

        override def toString: String = this match
            case SomeError => "SomeError"

        override def getMessage: String = this match
            case SomeError => "An error occurred during deployment"

    /** @param utxosToSpend
      *   utxos to pay the tx fees and storage fees for a script deployed AKA min ADA, should be
      *   key-locked. The change is sent back to the address of the first utxo.
      * @param scriptToDeploy
      *   the script we are deploying
      */
    final case class Build(
        utxosToSpend: NonEmptyList[Utxo],
        scriptToDeploy: ScriptRef
    ) {

        def result(using config: Config): Either[SomeBuildError | Error, DeploymentTx] = {

            val burnAddress = mkBurnAddress(config.network)

            val refScriptOutput = TransactionOutput(
              address = burnAddress,
              value = Value.zero,
              datumOption = None,
              scriptRef = Some(scriptToDeploy)
            )
                .ensureMinAda(config)

            val changeOutput = TransactionOutput(
              address = utxosToSpend.head.output.address,
              value = Value.combine(utxosToSpend.toList.map(_.output.value))
                  - refScriptOutput.value
            )

            for {
                context <- build(
                  utxosToSpend.toList.map(u => Spend(u))
                      :+ Send(refScriptOutput)
                      :+ Send(changeOutput)
                )

                finalized <- context.finalizeContext(
                  diffHandler = Change.changeOutputDiffHandler(1),
                  validators = nonSigningValidators
                )
            } yield DeploymentTx(
              scriptDeployed = scriptToDeploy.script.scriptHash,
              refScriptUtxo = TransactionInput(finalized.transaction.id, 0),
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

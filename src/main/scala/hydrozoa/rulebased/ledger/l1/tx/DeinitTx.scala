package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.contextualscalus.Change
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{build, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.{Resolved, Unresolved}
import hydrozoa.rulebased.ledger.l1.tx.DeinitTxOps.Build.Error.*
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, *}
import monocle.*
import scalus.cardano.ledger.{BlockHeader as _, *}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.{SomeBuildError, *}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

final case class DeinitTx(
    treasuryUtxoSpent: RuleBasedTreasuryUtxo,
    override val tx: Transaction,
    override val txLens: Lens[DeinitTx, Transaction] = Focus[DeinitTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[DeinitTx] {
    override def transactionFamily: String = "Deinit"
}

/** The deinit tx spends an empty (i.e. not containing any l2 utxos) treasury utxo, distributing the
  * residual _head equity_ according to peers' shares. If a share happens to be less than min ada,
  * it goes for the fees.
  *
  * Since the treasury is locked at the Plutus script it requires the collateral. This collateral is
  * used for fees as well, which simplifies building - we don't need to subtract fees from the
  * treasury.
  *
  * When it comes to multi-signing, all nodes cannot be built _exactly_ the same transaction since
  * every will use their own collateral. This should be addressed when implementing automatic
  * signing if we decide to have it, for now we expect this operation to be done manually.
  *
  * All head tokens under the head's policy id (and only those) should be burnt.
  */
object DeinitTx {
    export DeinitTxOps.{Build, Config}
}

private object DeinitTxOps {
    type Config = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
        HasTokenNames & ScriptReferenceUtxos.Section

    object Build {
        // TODO add `getMessage`
        enum Error extends Throwable:
            case TreasuryShouldBeResolved
            case TreasuryShouldBeEmpty
            case NoHeadTokensFound

    }

    final case class Build(
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: CollateralUtxo,
    ) {

        def result(using config: Config): Either[SomeBuildError | Build.Error, DeinitTx] = {
            for {
                _ <- checkTreasury
                result <- buildDeinitTx
            } yield result
        }

        val checkTreasury: Either[Build.Error, Unit] =

            // TODO use G1.generatorCompressed once it's here
            val g1bs =
                hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

            for {
                resolved <- treasuryUtxo.treasuryOutput.datum match {
                    case _: Unresolved => Left(TreasuryShouldBeResolved)
                    case d: Resolved   => Right(d)
                }
                _ <- Either.cond(resolved.evacuationActive == g1bs, (), TreasuryShouldBeEmpty)

            } yield ()

        def buildDeinitTx(using config: Config): Either[SomeBuildError | Build.Error, DeinitTx] = {

            for {
                context <- build(
                  List(
                    config.referenceTreasury,
                    // Spend the treasury utxo
                    treasuryUtxo.spendAttached(TreasuryRedeemer.Deinit),
                    // Fees are covered by the collateral to simplify the balancing
                    collateralUtxo.spend,
                    collateralUtxo.add,
                    // Send collateral back as the first output
                    collateralUtxo.collateralOutput.send
                  ) ++ treasuryUtxo.treasuryOutput.burnHeadTokens
                )

                finalized <- context
                    .finalizeContext(
                      diffHandler = Change.changeOutputDiffHandler(
                        0
                      ), // the collateral sent back
                      validators = nonSigningValidators
                    )

            } yield DeinitTx(
              treasuryUtxoSpent = treasuryUtxo,
              tx = finalized.transaction
            )
        }
    }
}

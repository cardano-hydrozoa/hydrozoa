package hydrozoa.multisig.ledger.eutxol2

import cats.data.EitherT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.multisig.ledger.eutxol2.L2TxFixtures.*
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.l2.L2LedgerError
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage

/** Evacuation purity at the [[EutxoL2Ledger.sendApplyTransaction]] level: the evacuation diffs and
  * payout obligations a minting transaction produces never carry transient bundles — every `Update`
  * obligation holds the projected (main-compartment) output value, and a withdrawal of the freed
  * backing value is token-free.
  */
class TransientTokensEvacuationPurityTest extends AnyFunSuite {

    /** Unwrap the ledger reply, failing with the wrapped message (not just the class name). */
    private def orFail[A](
        reply: EitherT[IO, L2LedgerError, A]
    ): IO[A] =
        reply.value.flatMap {
            case Right(a)    => IO.pure(a)
            case Left(error) => IO.raiseError(RuntimeException(error.toString))
        }

    test("mint + withdraw: diffs and payouts stay free of transient bundles") {
        val program = for {
            ledger <- EutxoL2Ledger(ledgerConfig)
            genesisState <- ledger.peekState
            _ = assert(
              genesisState.activeUtxos.nonEmpty,
              "fixture seed produced an empty initial L2 state; pick a different seed"
            )
            pot = genesisState.activeUtxos.maxBy(_._2.value.coin.value)
            // The pot may carry L1-native tokens; they must survive projection untouched while
            // the transient DEMO never reaches a diff or payout.
            potValue = pot._2.value

            // Mint 5 DEMO onto the pot's successor (L2-bound, declared transient).
            mintTx = buildSignedL2Tx(
              spends = List(pot),
              sends = List(
                (Babbage(peerAddress, potValue + Value(Coin.zero, mkDemoBundle(5))), 2)
              ),
              mints = List(mkMintDemoStep(5)),
              transientOutputs = Map(0 -> mkDemoBundle(5))
            )
            mintResult <- orFail(ledger.sendApplyTransaction(mkApplyTransaction(1, 1, mintTx)))
            (mintDiffs, mintPayouts) = mintResult

            overlaidUtxoId = TransactionInput(mintTx.id, 0)

            // Burn the 5 DEMO and withdraw the freed backing value to L1.
            burnAndWithdrawTx = buildSignedL2Tx(
              spends = List(
                overlaidUtxoId ->
                    Babbage(peerAddress, potValue + Value(Coin.zero, mkDemoBundle(5)))
              ),
              sends = List((Babbage(peerAddress, potValue), 1)),
              mints = List(mkMintDemoStep(-5))
            )
            burnResult <- orFail(
              ledger.sendApplyTransaction(mkApplyTransaction(2, 2, burnAndWithdrawTx))
            )
            (burnDiffs, burnPayouts) = burnResult

            finalState <- ledger.peekState
        } yield {
            val mintUpdates = mintDiffs.collect { case u: EvacuationDiff.Update => u }
            val _ = assert(mintPayouts.isEmpty, "the mint transaction withdraws nothing")
            val _ = assert(
              mintUpdates.nonEmpty && mintUpdates.forall(update =>
                  update.value.utxo.value.value == pot._2.value
              ),
              s"mint diffs must carry the projected value (pot value, no DEMO), got: $mintDiffs"
            )

            val burnUpdates = burnDiffs.collect { case u: EvacuationDiff.Update => u }
            val _ = assert(burnUpdates.isEmpty, "the withdrawal adds no L2 utxo")
            val _ = assert(
              burnDiffs.exists {
                  case EvacuationDiff.Delete(_) => true
                  case _                        => false
              },
              "spending the overlaid utxo must delete its evacuation entry"
            )
            val _ = assert(
              burnPayouts.nonEmpty && burnPayouts.forall(_.utxo.value.value == pot._2.value),
              s"the withdrawal payout must equal the pot value (no DEMO), got: $burnPayouts"
            )
            assert(
              finalState.transientTokens.isEmpty,
              "the overlay must be empty after the burn"
            )
        }
        program.unsafeRunSync()
    }
}

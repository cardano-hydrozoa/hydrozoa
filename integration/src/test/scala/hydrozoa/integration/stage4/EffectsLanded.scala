package hydrozoa.integration.stage4

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, info}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
import org.scalacheck.Prop
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.ledger.TransactionHash

/** Stage4 "effects landed" assertion: every backbone tx the slow cycle produced (settlement /
  * finalization / init plus dependent rollouts) lands on L1.
  *
  * Backend-agnostic via [[CardanoBackend.isTxKnown]]. Rule-based fallback is out of scope — see the
  * package docstring; the suite-level fallback signal fails the test before this property runs.
  *
  * ==Out of scope==
  *
  *   - Refunds: user-submitted, not produced by `CardanoLiaison`.
  *   - Standalone evac commitments: header signatures, not L1 txs.
  *   - Strong (model-predicted) backbone: this only verifies what the slow cycle produced landed;
  *     it cannot catch "the slow side never produced an expected settlement".
  */
object EffectsLanded {

    /** A single block's L1 expectation: every tx in `happyTxs` must be `isTxKnown`. */
    final case class BlockExpectation(label: String, happyTxs: List[TransactionHash])

    /** Walk observed stacks into per-block expectations. `Minor` partitions are skipped — they
      * carry no main L1 tx (sec is a header commitment, refunds are user-submitted).
      */
    def expectations(stacks: Seq[Stack.HardConfirmed]): List[BlockExpectation] =
        stacks.toList.flatMap { stack =>
            val stackNum = stack.brief.stackNum: Int
            stack.effects match {
                case i: StackEffects.HardConfirmed.Initial =>
                    List(
                      BlockExpectation(
                        label = s"stack#$stackNum init",
                        happyTxs = List(i.initializationTx.tx.id),
                      )
                    )
                case r: StackEffects.HardConfirmed.Regular =>
                    r.partitions.toList.zipWithIndex.flatMap {
                        case (m: PartitionEffects.Major[?], partIdx) =>
                            List(
                              BlockExpectation(
                                label = s"stack#$stackNum,p$partIdx major",
                                happyTxs = m.settlement.tx.id :: m.rollouts.map(_.tx.id),
                              )
                            )
                        case (f: PartitionEffects.Final, partIdx) =>
                            List(
                              BlockExpectation(
                                label = s"stack#$stackNum,p$partIdx final",
                                happyTxs = f.finalization.tx.id :: f.rollouts.map(_.tx.id),
                              )
                            )
                        case (_: PartitionEffects.Minor[?], _) => Nil
                    }
            }
        }

    enum BlockOutcome:
        case Happy
        case Pending

    final case class BlockResult(
        expectation: BlockExpectation,
        outcome: BlockOutcome,
        happyKnown: Int,
        happyTotal: Int,
    )

    private def evaluateOnce(
        backend: CardanoBackend[IO],
        exps: List[BlockExpectation],
    ): IO[List[BlockResult]] =
        exps.traverse { e =>
            for {
                happyResults <- e.happyTxs.traverse(backend.isTxKnown)
                happyKnown = happyResults.count(_.contains(true))
                happyTotal = e.happyTxs.size
                outcome =
                    if happyKnown == happyTotal then BlockOutcome.Happy
                    else BlockOutcome.Pending
            } yield BlockResult(e, outcome, happyKnown, happyTotal)
        }

    private def evaluateAgainst(
        landed: Set[TransactionHash],
        exps: List[BlockExpectation],
    ): List[BlockResult] =
        exps.map { e =>
            val happyKnown = e.happyTxs.count(landed.contains)
            val happyTotal = e.happyTxs.size
            val outcome =
                if happyKnown == happyTotal then BlockOutcome.Happy
                else BlockOutcome.Pending
            BlockResult(e, outcome, happyKnown, happyTotal)
        }

    /** Predicate driving `effectsLandedSignal`: every expectation has reached `Happy`. */
    def isComplete(landed: Set[TransactionHash], exps: List[BlockExpectation]): Boolean =
        evaluateAgainst(landed, exps).forall(_.outcome == BlockOutcome.Happy)

    /** Poll the backend until every expectation is `Happy` or the attempt budget is exhausted. */
    def poll(
        backend: CardanoBackend[IO],
        exps: List[BlockExpectation],
        attempts: Int,
        sleep: FiniteDuration,
    ): IO[List[BlockResult]] = {
        def loop(attempt: Int): IO[List[BlockResult]] =
            evaluateOnce(backend, exps).flatMap { results =>
                val allHappy = results.forall(_.outcome == BlockOutcome.Happy)
                if allHappy || attempt + 1 >= attempts then IO.pure(results)
                else IO.sleep(sleep) >> loop(attempt + 1)
            }
        loop(0)
    }

    final case class EffectsStats(
        blocks: Int,
        happyBlocks: Int,
        pendingBlocks: Int,
        happyTxsKnown: Int,
        happyTxsTotal: Int,
    )

    object EffectsStats {
        def of(results: List[BlockResult]): EffectsStats =
            EffectsStats(
              blocks = results.size,
              happyBlocks = results.count(_.outcome == BlockOutcome.Happy),
              pendingBlocks = results.count(_.outcome == BlockOutcome.Pending),
              happyTxsKnown = results.map(_.happyKnown).sum,
              happyTxsTotal = results.map(_.happyTotal).sum,
            )
    }

    /** Render the per-block effects table + stats footer (visual style matches
      * [[Stage4Suite#traceBlockTable]] / [[Stage4Suite#traceStackTable]]).
      */
    def renderEffectsTable(results: List[BlockResult]): String = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Effects landed".padTo(colWidth, ' ')} |"
        val rows = results.map { r =>
            val outcomeStr = r.outcome match {
                case BlockOutcome.Happy   => "Happy"
                case BlockOutcome.Pending => "Pending"
            }
            val label =
                s"${r.expectation.label}: $outcomeStr (happy=${r.happyKnown}/${r.happyTotal})"
            s"| ${label.take(colWidth).padTo(colWidth, ' ')} |"
        }

        val s = EffectsStats.of(results)
        val totals = s"Blocks: ${s.blocks}  Happy=${s.happyBlocks}  Pending=${s.pendingBlocks}"
        val txCounts = s"Happy txs landed: ${s.happyTxsKnown}/${s.happyTxsTotal}"
        val legend =
            "Legend: Happy=all happy-path txs on L1; Pending=neither completed within budget"

        (divider :: header :: divider :: rows ::: divider :: totals :: txCounts :: legend :: Nil)
            .mkString("\n", "\n", "")
    }

    def traceEffectsTable(
        results: List[BlockResult],
        log: ContraTracer[IO, Slf4jMsg]
    ): IO[Unit] =
        log.info(renderEffectsTable(results))

    /** Build the property directly from observed stacks + backend. Vacuous when the observed stack
      * list is empty — [[Stage4Suite.propStackCoverage]] catches that case, not this one.
      */
    def propEffectsLanded(
        stacks: Seq[Stack.HardConfirmed],
        backend: CardanoBackend[IO],
        log: ContraTracer[IO, Slf4jMsg],
        attempts: Int = 10,
        sleep: FiniteDuration = 1.second,
    ): IO[Prop] = {
        val exps = expectations(stacks)
        if exps.isEmpty then IO.pure(Prop.passed)
        else
            for {
                results <- poll(backend, exps, attempts, sleep)
                _ <- traceEffectsTable(results, log)
                pending = results.filter(_.outcome == BlockOutcome.Pending)
                diag = pending.map(r =>
                    s"${r.expectation.label}: happy ${r.happyKnown}/${r.happyTotal} known"
                )
            } yield Prop(pending.isEmpty) :|
                s"effects landed: ${pending.size} of ${exps.size} block(s) " +
                "did not complete on the happy path:\n" + diag.mkString("\n")
    }
}

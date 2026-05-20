package hydrozoa.integration.stage4

import cats.effect.{IO, IOLocal}
import cats.syntax.all.*
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
import org.scalacheck.Prop
import scalus.cardano.ledger.TransactionHash

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Stage4-local "effects landed" assertion.
  *
  * Backend-agnostic by construction ([[CardanoBackend.isTxKnown]] is the only L1 contact point), so
  * a future stage4 swap to Yaci or a public testnet is just a different `CardanoBackend[IO]`. Kept
  * under `stage4` rather than a shared util because stage1 doesn't produce stacks (it stubs the
  * slow side); the surface area is currently stage4-only.
  *
  * == Per-block happy-or-fallback disjunction ==
  *
  * Walks the observed [[Stack.HardConfirmed]] sequence and splits it into per-block expectations.
  * Each block has two mutually-exclusive completion paths and the assertion accepts either:
  *
  *   - **Happy** — settlement / finalization + every rollout landed on L1
  *   - **Fallback** — the competing fallback tx landed on L1 (only for Major blocks and the
  *     initial bootstrap; finals have no fallback partner)
  *
  * The poll loop retries until every block has completed via one of the two paths or the attempt
  * budget runs out. The disjunction means the assertion does NOT require model-time knowledge of
  * whether the happy or fallback path "should" have fired — it accepts whichever the protocol
  * actually took. That sidesteps stage4's lack of an `AfterCompetingFallbackStartTime` analogue.
  *
  * Stage4 mock backend resolves instantly and never triggers the fallback path in current
  * scenarios, so in practice every block completes via the happy path on the first attempt. The
  * fallback branch is exercised the moment we add happy-path-expiration commands or swap in a
  * real-clock backend.
  *
  * == Out of scope ==
  *
  *   - Refunds: user-submitted, not produced by the protocol's CardanoLiaison. Not checked.
  *   - Standalone evac commitments: header signatures, not L1 txs.
  *   - Strong (model-predicted) backbone: this assertion only verifies what the slow cycle
  *     produced actually landed. It cannot catch "the slow side never produced an expected
  *     settlement".
  */
object EffectsLanded {

    /** A single block's L1 expectation. Exactly one of the two paths must be observed on the
      * backend for the block to be considered "landed".
      *
      * @param label
      *   human-readable identifier for diagnostics
      * @param happyTxs
      *   all txs that constitute happy-path completion (settlement / finalization / init plus
      *   every dependent rollout); EVERY one must be `isTxKnown` for the happy path to be
      *   considered complete
      * @param fallback
      *   the alternate completion tx if the happy path doesn't fire; `None` for Final partitions
      */
    final case class BlockExpectation(
        label: String,
        happyTxs: List[TransactionHash],
        fallback: Option[TransactionHash],
    )

    /** Walks observed stacks, projecting each `Major` / `Final` partition (and the `Initial`
      * stack) into one [[BlockExpectation]]. `Minor` partitions are skipped — they carry no main
      * L1 tx (sec is a header commitment, refunds are user-submitted).
      */
    def expectations(stacks: Seq[Stack.HardConfirmed]): List[BlockExpectation] =
        stacks.toList.flatMap { stack =>
            val stackNum = stack.unsigned.brief.stackNum: Int
            stack.effects match {
                case i: StackEffects.HardConfirmed.Initial =>
                    List(
                      BlockExpectation(
                        label = s"stack#$stackNum init",
                        happyTxs = List(i.initializationTx.tx.id),
                        fallback = Some(i.fallbackTx.tx.id),
                      )
                    )
                case r: StackEffects.HardConfirmed.Regular =>
                    r.partitions.toList.zipWithIndex.flatMap {
                        case (m: PartitionEffects.Major[?], partIdx) =>
                            List(
                              BlockExpectation(
                                label = s"stack#$stackNum,p$partIdx major",
                                happyTxs =
                                    m.settlement.tx.id :: m.rollouts.map(_.tx.id),
                                fallback = Some(m.fallback.tx.id),
                              )
                            )
                        case (f: PartitionEffects.Final, partIdx) =>
                            List(
                              BlockExpectation(
                                label = s"stack#$stackNum,p$partIdx final",
                                happyTxs =
                                    f.finalization.tx.id :: f.rollouts.map(_.tx.id),
                                fallback = None,
                              )
                            )
                        case (_: PartitionEffects.Minor[?], _) => Nil
                    }
            }
        }

    /** Outcome of one block expectation after polling. */
    enum BlockOutcome:
        case Happy
        case Fallback
        case Pending

    /** Per-block result enriched with the raw landed counts (for stats output / diagnostics). */
    final case class BlockResult(
        expectation: BlockExpectation,
        outcome: BlockOutcome,
        happyKnown: Int,
        happyTotal: Int,
        fallbackKnown: Option[Boolean],
    )

    /** Single-pass evaluation: for each expectation, returns the outcome with happy-tx and
      * fallback-tx landed counts.
      */
    private def evaluateOnce(
        backend: CardanoBackend[IO],
        exps: List[BlockExpectation],
    ): IO[List[BlockResult]] =
        exps.traverse { e =>
            for {
                happyResults <- e.happyTxs.traverse(backend.isTxKnown)
                fallbackResult <- e.fallback.traverse(backend.isTxKnown)
                happyKnown = happyResults.count(_.contains(true))
                happyTotal = e.happyTxs.size
                fallbackKnown: Option[Boolean] = fallbackResult.map(_.contains(true))
                outcome =
                    if happyKnown == happyTotal then BlockOutcome.Happy
                    else if fallbackKnown.contains(true) then BlockOutcome.Fallback
                    else BlockOutcome.Pending
            } yield BlockResult(e, outcome, happyKnown, happyTotal, fallbackKnown)
        }

    /** Poll the backend until every expectation has reached `Happy` or `Fallback`, or the attempt
      * budget is exhausted. Sleep between attempts is virtual under TestControl, wall-clock
      * otherwise — caller picks the budget appropriate to the backend.
      */
    def poll(
        backend: CardanoBackend[IO],
        exps: List[BlockExpectation],
        attempts: Int,
        sleep: FiniteDuration,
    ): IO[List[BlockResult]] = {
        def loop(attempt: Int): IO[List[BlockResult]] =
            evaluateOnce(backend, exps).flatMap { results =>
                val allSettled = results.forall(_.outcome != BlockOutcome.Pending)
                if allSettled || attempt + 1 >= attempts then IO.pure(results)
                else IO.sleep(sleep) >> loop(attempt + 1)
            }
        loop(0)
    }

    /** Aggregate stats across every per-block result. */
    final case class EffectsStats(
        blocks: Int,
        happyBlocks: Int,
        fallbackBlocks: Int,
        pendingBlocks: Int,
        happyTxsKnown: Int,
        happyTxsTotal: Int,
        fallbackTxsKnown: Int,
        fallbackTxsTotal: Int,
    )

    object EffectsStats {
        def of(results: List[BlockResult]): EffectsStats = {
            val happyBlocks = results.count(_.outcome == BlockOutcome.Happy)
            val fallbackBlocks = results.count(_.outcome == BlockOutcome.Fallback)
            val pendingBlocks = results.count(_.outcome == BlockOutcome.Pending)
            val happyTxsKnown = results.map(_.happyKnown).sum
            val happyTxsTotal = results.map(_.happyTotal).sum
            val fallbackTxsKnown = results.count(_.fallbackKnown.contains(true))
            val fallbackTxsTotal = results.count(_.fallbackKnown.isDefined)
            EffectsStats(
              blocks = results.size,
              happyBlocks = happyBlocks,
              fallbackBlocks = fallbackBlocks,
              pendingBlocks = pendingBlocks,
              happyTxsKnown = happyTxsKnown,
              happyTxsTotal = happyTxsTotal,
              fallbackTxsKnown = fallbackTxsKnown,
              fallbackTxsTotal = fallbackTxsTotal,
            )
        }
    }

    /** Render the per-block effects table + aggregate stats footer as a single multi-line
      * string, matching the visual style of [[Stage4Suite#traceBlockTable]] /
      * [[Stage4Suite#traceStackTable]]. Kept as pure rendering so callers can route it to the
      * tracer (or anywhere else) — see [[traceEffectsTable]].
      */
    def renderEffectsTable(results: List[BlockResult]): String = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Effects landed".padTo(colWidth, ' ')} |"
        val rows = results.map { r =>
            val outcomeStr = r.outcome match {
                case BlockOutcome.Happy    => "Happy"
                case BlockOutcome.Fallback => "Fallback"
                case BlockOutcome.Pending  => "Pending"
            }
            val happyStr = s"happy=${r.happyKnown}/${r.happyTotal}"
            val fbStr = r.fallbackKnown match {
                case Some(true)  => "fb=1/1"
                case Some(false) => "fb=0/1"
                case None        => "fb=n/a"
            }
            val label = s"${r.expectation.label}: $outcomeStr ($happyStr, $fbStr)"
            s"| ${label.take(colWidth).padTo(colWidth, ' ')} |"
        }

        val s = EffectsStats.of(results)
        val totals = s"Blocks: ${s.blocks}  Happy=${s.happyBlocks}  " +
            s"Fallback=${s.fallbackBlocks}  Pending=${s.pendingBlocks}"
        val txCounts = s"Happy txs landed: ${s.happyTxsKnown}/${s.happyTxsTotal}  " +
            s"Fallback txs landed: ${s.fallbackTxsKnown}/${s.fallbackTxsTotal}"
        val legend =
            "Legend: Happy=all happy-path txs on L1; Fallback=competing fallback on L1; " +
                "Pending=neither completed within budget; fb=n/a means Final (no fallback partner)"

        (divider :: header :: divider :: rows ::: divider :: totals :: txCounts :: legend :: Nil)
            .mkString("\n", "\n", "")
    }

    /** Emit the rendered table to the tracer at INFO level so it lands in the log file
      * alongside the rest of the harness output. Single trace event keeps the table contiguous.
      */
    def traceEffectsTable(results: List[BlockResult])(using IOLocal[Tracer]): IO[Unit] =
        Tracer.info(renderEffectsTable(results))

    /** Build the property directly from observed stacks + backend. Vacuous (true) if the
      * observed stack list is empty — [[Stage4Suite.propStackCoverage]] catches that case, not
      * this one.
      *
      * Also emits the per-block effects table + aggregate stats footer via
      * [[traceEffectsTable]], matching the stage4 block / stack tables.
      */
    def propEffectsLanded(
        stacks: Seq[Stack.HardConfirmed],
        backend: CardanoBackend[IO],
        attempts: Int = 10,
        sleep: FiniteDuration = 1.second,
    )(using IOLocal[Tracer]): IO[Prop] = {
        val exps = expectations(stacks)
        if exps.isEmpty then IO.pure(Prop.passed)
        else
            for {
                results <- poll(backend, exps, attempts, sleep)
                _ <- traceEffectsTable(results)
                pending = results.filter(_.outcome == BlockOutcome.Pending)
                diag = pending.map(r =>
                    s"${r.expectation.label}: " +
                        s"happy ${r.happyKnown}/${r.happyTotal} known; " +
                        (r.fallbackKnown match {
                            case Some(true)  => "fallback landed (unexpected Pending)"
                            case Some(false) => "fallback not landed"
                            case None        => "no fallback (Final block)"
                        })
                )
            } yield Prop(pending.isEmpty) :|
                s"effects landed: ${pending.size} of ${exps.size} block(s) " +
                s"with neither happy nor fallback completion:\n" + diag.mkString("\n")
    }
}

package hydrozoa.integration.stage4

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, info}
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
  * == Per-transition happy-or-fallback disjunction ==
  *
  * Walks the observed [[Stack.HardConfirmed]] sequence into ordered backbone steps and checks
  * each *treasury transition*. Each transition has two mutually-exclusive completion paths and
  * the assertion accepts either:
  *
  *   - **Happy** — this step's settlement / finalization + every rollout landed on L1
  *   - **Fallback** — the PREVIOUS step's competing fallback landed on L1
  *
  * The off-by-one is the crux: the competing fallback bundled with major block N spends N's
  * treasury *output*, so it races settlement N+1 (which spends the same utxo), not settlement N.
  * When settlement N+1 misses its window the liaison submits fallback N. So the disjunction for
  * the transition into step k is `settlement(k)` OR `fallback(k-1)` — see [[expectations]]. The
  * genesis init step has no predecessor and therefore no competing fallback.
  *
  * The poll loop retries until every block has completed via one of the two paths or the attempt
  * budget runs out. The disjunction means the assertion does NOT require model-time knowledge of
  * whether the happy or fallback path "should" have fired — it accepts whichever the protocol
  * actually took. That sidesteps stage4's lack of an `AfterCompetingFallbackStartTime` analogue.
  *
  * == Fallback is terminal ==
  *
  * At most one fallback is ever submitted: it moves the head into the rule-based dispute regime,
  * after which no further happy-path effects are produced. So a block that sits *after* the first
  * observed fallback is **Moot**, not a failure — its happy-path txs were never meant to land.
  * The property therefore only requires that every block up to and including the first fallback
  * completed (Happy, or the fallback itself); everything past it is ignored. Throttling the slow
  * cycle (rate limiter) can push effects past their L1 validity windows and legitimately trigger
  * this fallback, so the Moot tail is expected, not a bug.
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
      *   the competing fallback for this transition — the PREVIOUS backbone step's fallback,
      *   which races this step's happy tx for the same treasury utxo. `None` for the genesis
      *   init step (no predecessor).
      */
    final case class BlockExpectation(
        label: String,
        happyTxs: List[TransactionHash],
        fallback: Option[TransactionHash],
    )

    /** One backbone step (init / settlement / finalization) with its own happy txs and the
      * competing fallback it *produces*. That fallback spends this step's treasury output, so it
      * races the NEXT step's happy tx — it is attributed to the next [[BlockExpectation]], not
      * this one (see [[expectations]]).
      */
    private final case class Backbone(
        label: String,
        happyTxs: List[TransactionHash],
        ownFallback: Option[TransactionHash],
    )

    /** Walks observed stacks into the ordered backbone steps. `Minor` partitions are skipped —
      * they carry no main L1 tx (sec is a header commitment, refunds are user-submitted).
      */
    private def backboneSteps(stacks: Seq[Stack.HardConfirmed]): List[Backbone] =
        stacks.toList.flatMap { stack =>
            val stackNum = stack.brief.stackNum: Int
            stack.effects match {
                case i: StackEffects.HardConfirmed.Initial =>
                    List(
                      Backbone(
                        label = s"stack#$stackNum init",
                        happyTxs = List(i.initializationTx.tx.id),
                        ownFallback = Some(i.fallbackTx.tx.id),
                      )
                    )
                case r: StackEffects.HardConfirmed.Regular =>
                    r.partitions.toList.zipWithIndex.flatMap {
                        case (m: PartitionEffects.Major[?], partIdx) =>
                            List(
                              Backbone(
                                label = s"stack#$stackNum,p$partIdx major",
                                happyTxs = m.settlement.tx.id :: m.rollouts.map(_.tx.id),
                                ownFallback = Some(m.fallback.tx.id),
                              )
                            )
                        case (f: PartitionEffects.Final, partIdx) =>
                            List(
                              Backbone(
                                label = s"stack#$stackNum,p$partIdx final",
                                happyTxs = f.finalization.tx.id :: f.rollouts.map(_.tx.id),
                                ownFallback = None,
                              )
                            )
                        case (_: PartitionEffects.Minor[?], _) => Nil
                    }
            }
        }

    /** Project the observed backbone into per-transition expectations.
      *
      * The transition *into* step `k` is resolved by EITHER step `k`'s own happy tx (settlement /
      * finalization landed) OR the PREVIOUS step's competing fallback (`step k-1`'s fallback,
      * which spends the same treasury input step `k`'s settlement would). So each expectation
      * pairs `step(k).happyTxs` with `step(k-1).ownFallback` — the fallback is offset one step
      * behind the settlement it races. The first step (genesis init) has no predecessor, hence no
      * competing fallback.
      */
    def expectations(stacks: Seq[Stack.HardConfirmed]): List[BlockExpectation] = {
        val steps = backboneSteps(stacks)
        steps.zipWithIndex.map { case (step, k) =>
            val competingFallback = if k == 0 then None else steps(k - 1).ownFallback
            BlockExpectation(step.label, step.happyTxs, competingFallback)
        }
    }

    /** Outcome of one block expectation after polling.
      *
      *   - `Happy` / `Fallback` — one of the two completion paths landed on L1.
      *   - `Pending` — neither landed; a genuine miss (only meaningful before any fallback).
      *   - `Moot` — this block sits *after* the first fallback. A fallback is terminal: the head
      *     has exited to the rule-based regime, so no further happy-path effects are submitted and
      *     these blocks are irrelevant, not failures.
      */
    enum BlockOutcome:
        case Happy
        case Fallback
        case Pending
        case Moot

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

    /** The block expectations that still matter for pass/fail: everything up to and including the
      * first observed fallback. A fallback is terminal (the head exits to the rule-based regime),
      * so blocks after it are irrelevant. With no fallback, all results are relevant.
      */
    private def relevantPrefix(results: List[BlockResult]): List[BlockResult] =
        results.indexWhere(_.outcome == BlockOutcome.Fallback) match {
            case -1 => results
            case k  => results.take(k + 1)
        }

    /** Poll the backend until every *relevant* expectation has reached `Happy` or `Fallback`, or
      * the attempt budget is exhausted. "Relevant" stops at the first fallback (see
      * [[relevantPrefix]]) — blocks past it never land happy-path effects, so they must not keep
      * the poll spinning. Sleep between attempts is virtual under TestControl, wall-clock
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
                val allSettled = relevantPrefix(results).forall(_.outcome != BlockOutcome.Pending)
                if allSettled || attempt + 1 >= attempts then IO.pure(results)
                else IO.sleep(sleep) >> loop(attempt + 1)
            }
        loop(0)
    }

    /** Relabel every block after the first fallback as `Moot` — a fallback is terminal, so those
      * blocks' (un)landed happy-path effects are irrelevant. Leaves results unchanged if no
      * fallback was observed.
      */
    private def mootAfterFallback(results: List[BlockResult]): List[BlockResult] =
        results.indexWhere(_.outcome == BlockOutcome.Fallback) match {
            case -1 => results
            case k =>
                results.zipWithIndex.map {
                    case (r, i) if i > k => r.copy(outcome = BlockOutcome.Moot)
                    case (r, _)          => r
                }
        }

    /** Aggregate stats across every per-block result. */
    final case class EffectsStats(
        blocks: Int,
        happyBlocks: Int,
        fallbackBlocks: Int,
        pendingBlocks: Int,
        mootBlocks: Int,
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
            val mootBlocks = results.count(_.outcome == BlockOutcome.Moot)
            val happyTxsKnown = results.map(_.happyKnown).sum
            val happyTxsTotal = results.map(_.happyTotal).sum
            val fallbackTxsKnown = results.count(_.fallbackKnown.contains(true))
            val fallbackTxsTotal = results.count(_.fallbackKnown.isDefined)
            EffectsStats(
              blocks = results.size,
              happyBlocks = happyBlocks,
              fallbackBlocks = fallbackBlocks,
              pendingBlocks = pendingBlocks,
              mootBlocks = mootBlocks,
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
                case BlockOutcome.Moot     => "Moot"
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
            s"Fallback=${s.fallbackBlocks}  Pending=${s.pendingBlocks}  Moot=${s.mootBlocks}"
        val txCounts = s"Happy txs landed: ${s.happyTxsKnown}/${s.happyTxsTotal}  " +
            s"Fallback txs landed: ${s.fallbackTxsKnown}/${s.fallbackTxsTotal}"
        val legend =
            "Legend: Happy=all happy-path txs on L1; Fallback=competing fallback on L1 (terminal); " +
                "Pending=neither completed within budget; Moot=after a fallback (rule-based " +
                "regime, irrelevant); fb=n/a means Final (no fallback partner)"

        (divider :: header :: divider :: rows ::: divider :: totals :: txCounts :: legend :: Nil)
            .mkString("\n", "\n", "")
    }

    /** Emit the rendered table to the tracer at INFO level so it lands in the log file
      * alongside the rest of the harness output. Single trace event keeps the table contiguous.
      */
    def traceEffectsTable(
        results: List[BlockResult],
        log: ContraTracer[IO, Slf4jMsg]
    ): IO[Unit] =
        log.info(renderEffectsTable(results))

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
        log: ContraTracer[IO, Slf4jMsg],
        attempts: Int = 10,
        sleep: FiniteDuration = 1.second,
    ): IO[Prop] = {
        val exps = expectations(stacks)
        if exps.isEmpty then IO.pure(Prop.passed)
        else
            for {
                polled <- poll(backend, exps, attempts, sleep)
                // A fallback is terminal: blocks after it are Moot, not failures.
                results = mootAfterFallback(polled)
                _ <- traceEffectsTable(results, log)
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
                s"effects landed: ${pending.size} of ${exps.size} block(s) before any fallback " +
                s"completed via neither happy nor fallback path:\n" + diag.mkString("\n")
    }
}

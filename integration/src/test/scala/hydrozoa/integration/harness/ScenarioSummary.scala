package hydrozoa.integration.harness

import cats.effect.{IO, Ref, Resource}
import hydrozoa.lib.logging.{ContraTracer, LogEvent, Slf4jTracer}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.{CommonChildEvent, LifecycleEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.typelevel.paiges.Doc
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.TransactionHash

/** Run-scoped scenario summarizer for the multi-peer dispute suites.
  *
  * This is a THIRD, distinct kind of tracer — not to be confused with the two it sits beside:
  *   - `RbrMbtSuite.observerTracer` is a *test-execution* tracer: it completes `Deferred`s the
  *     suite awaits, so it is load-bearing for the test running at all.
  *   - [[DiagnosticTracers]] are *developer* tracers: optional, richer-than-log-level inline slf4j
  *     detail toggled in during an investigation.
  *
  * A [[ScenarioSummary]] is neither. It is self-contained and run-scoped: it owns its own
  * accumulator, quietly collects only the events that reflect *successful* actions across the whole
  * run, and prints ONE rendered summary at the end (on [[resource]] release). It gates no control
  * flow and emits nothing inline. A scenario opts in by composing [[tracer]] via `|+|` into the
  * harness tracer; everything else — collection, folding, the end-of-run print — belongs to the
  * summarizer, decoupled from how the test gates itself or what a developer toggles on.
  *
  * The design principle for "success": every rule-based tx moves Building → Submitting →
  * (`SubmitSuccess` | `Backend.ErrorSubmittingTx`), and under contention many peers attempt while
  * one lands. The summary keeps the terminal *landed* variant as the success signal and collapses
  * the attempts into a single "attempts → landed" contention line — turning the log's expected-race
  * noise into one number.
  */
final class ScenarioSummary private (label: String, events: Ref[IO, Vector[ScenarioSummary.Entry]]):
    import ScenarioSummary.*

    /** The capture sink: match only success/milestone variants, stamp with the (virtual or wall)
      * clock, and append. Compose into the harness tracer with `|+|`. Events it does not recognise
      * cost nothing beyond the pattern match — no clock read, no `Ref` update.
      */
    val tracer: ContraTracer[IO, MultiPeerHeadHarness.Event] =
        def capture(peer: PeerId, child: Any): IO[Unit] =
            classify(child) match
                case None => IO.unit
                case Some(ev) =>
                    IO.realTime.flatMap(now => events.update(_ :+ Entry(now, peer, ev)))
        ContraTracer[IO, MultiPeerHeadHarness.Event] {
            case MultiPeerHeadHarness.Event.Head(peerNum, evt) => capture(PeerId.Head(peerNum), evt)
            case MultiPeerHeadHarness.Event.Coil(coilNum, evt) => capture(PeerId.Coil(coilNum), evt)
        }

    /** Fold the accumulator and emit the summary once, via the `Scenario.Summary` slf4j route. */
    def render: IO[Unit] =
        events.get.flatMap { evs =>
            val text =
                if evs.isEmpty then s"=== scenario summary: $label ===\n(no events captured)"
                else renderDoc(label, evs).render(200)
            Slf4jTracer.sink.traceWith(LogEvent.From(Map.empty, "Scenario.Summary").info(text))
        }

object ScenarioSummary:

    /** Acquire a summarizer (fresh accumulator); its release renders the summary. Acquire it
      * *first* in the SUT resource so its finalizer runs *last* — the summary lands at the very end
      * of the run, after harness teardown.
      */
    def resource(label: String): Resource[IO, ScenarioSummary] =
        Resource.make(
          Ref[IO].of(Vector.empty[Entry]).map(new ScenarioSummary(label, _))
        )(_.render)

    /** One captured success/milestone, tagged with the emitting peer and the clock time it was
      * observed (relative deltas are computed at render time against the earliest entry).
      */
    final case class Entry(at: FiniteDuration, peer: PeerId, event: ScenarioEvent)

    /** The distilled, summary-only vocabulary. Deliberately narrow: only the events that mark a
      * successful step, plus `TxAttempt` (kept solely to render the attempts→landed contention
      * line).
      */
    enum ScenarioEvent:
        // Multisig regime (pre-fallback): bring-up + consensus + L1 effects.
        case RegimeStarted
        case Bootstrapped
        case InitEffects
        case EffectsLearned(
            settlements: Int,
            fallbacks: Int,
            rollouts: Int,
            hasFinalization: Boolean
        )
        case SoftConfirmed(blockNum: BlockNumber, blockType: String, major: Int, minor: Int)
        case StackClosed(stackNum: StackNumber, isLeader: Boolean)
        case StackHardConfirmed(stackNum: StackNumber)
        case CommittedMap(version: BlockVersion.Full, size: Int)
        // The multisig→rule-based handoff, then the rule-based regime (dispute + evacuation).
        case FellBack(txId: TransactionHash)
        case TxAttempt(family: String)
        case TxLanded(family: String, txId: TransactionHash)
        case PayoutsLeft(n: Int)
        case Evacuated

    import ScenarioEvent.*

    /** Project a raw regime-manager child event to a summary event, or `None` to ignore it. Typed
      * as `Any` to sidestep the Head/Coil union (mirrors `RbrMbtSuite.observerTracer`).
      */
    private def classify(child: Any): Option[ScenarioEvent] = child match
        case LifecycleEvent.StartingActors =>
            Some(RegimeStarted)
        case CommonChildEvent.StackComposer(StackComposerEvent.InitialStackBootstrapped) =>
            Some(Bootstrapped)
        case CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.InitialStackEffectsLearned) =>
            Some(InitEffects)
        case CommonChildEvent.CardanoLiaison(
              CardanoLiaisonEvent.StackEffectsLearned(settlements, fallbacks, rollouts, hasFinal)
            ) =>
            Some(EffectsLearned(settlements, fallbacks, rollouts, hasFinal))
        case CommonChildEvent.CardanoLiaison(
              CardanoLiaisonEvent.FallbackToRuleBasedDispatched(txId)
            ) =>
            Some(FellBack(txId))
        case RuleBasedOnlyChildEvent.RuleBasedActor(RuleBasedActorEvent.Tx.Submitting(tx)) =>
            Some(TxAttempt(tx.transactionFamily))
        case RuleBasedOnlyChildEvent.RuleBasedActor(RuleBasedActorEvent.Tx.SubmitSuccess(tx)) =>
            Some(TxLanded(tx.transactionFamily, tx.tx.id))
        case RuleBasedOnlyChildEvent.RuleBasedActor(
              RuleBasedActorEvent.Evacuation.PayoutsLeft(n)
            ) =>
            Some(PayoutsLeft(n))
        case RuleBasedOnlyChildEvent.RuleBasedActor(RuleBasedActorEvent.Evacuation.NoMore) =>
            Some(Evacuated)
        case CommonChildEvent.SlowConsensusActor(
              SlowConsensusActorEvent.StackHardConfirmed(stack)
            ) =>
            Some(StackHardConfirmed(stack.stackNum))
        case CommonChildEvent.StackComposer(
              StackComposerEvent.StackClosed(stackNum, _, _, isLeader)
            ) =>
            Some(StackClosed(stackNum, isLeader))
        case CommonChildEvent.StackComposer(StackComposerEvent.CommittedMap(version, size)) =>
            Some(CommittedMap(version, size))
        case CommonChildEvent.FastConsensusActor(
              FastConsensusActorEvent.BlockSoftConfirmed(blockNum, blockType, major, minor)
            ) =>
            Some(SoftConfirmed(blockNum, blockType, major, minor))
        case _ => None

    // ------------------------------------------------------------------
    // Rendering (paiges Doc)
    // ------------------------------------------------------------------

    private def renderDoc(label: String, evs: Vector[Entry]): Doc =
        val t0 = evs.map(_.at).min
        def rel(at: FiniteDuration): String = f"${(at - t0).toMillis / 1000.0}%6.1fs"

        val title = Doc.text(s"=== scenario summary: $label ===")
        val counts = Doc.text(
          s"peers=${evs.map(_.peer).distinct.size}  events=${evs.size}  span=${rel(evs.map(_.at).max).trim}"
        )

        section(title, counts) +
            section(Doc.text("milestones"), milestones(evs, rel)) +
            section(Doc.text("per peer"), peerTable(evs)) +
            section(Doc.text("tx contention (attempts → landed)"), contention(evs)) +
            section(Doc.text("committed evacuation maps"), committedMaps(evs))

    /** A chronological timeline spanning both regimes: the multisig phase (regime up → bootstrap →
      * init/finalization L1 effects), the multisig→rule-based handoff, and the rule-based phase
      * (first landing of each dispute tx family, last peer to evacuate). Each row is the *first*
      * occurrence of its milestone (many fire once per peer), sorted by time.
      */
    private def milestones(evs: Vector[Entry], rel: FiniteDuration => String): Doc =
        val peersSeen = evs.map(_.peer).distinct.size
        def firstAt(p: ScenarioEvent => Boolean): Option[FiniteDuration] =
            evs.filter(e => p(e.event)).map(_.at).minOption

        val evacuated = evs.filter(_.event == Evacuated)
        val firstLandedByFamily = evs
            .collect { case e @ Entry(_, _, TxLanded(fam, _)) => fam -> e }
            .groupMapReduce(_._1)(_._2)((a, b) => if a.at <= b.at then a else b)

        // (time, label) pairs, gathered then sorted so multisig and rule-based milestones interleave
        // in true wall-clock order.
        val items: List[(FiniteDuration, String)] =
            firstAt(_ == RegimeStarted).map(_ -> s"multisig regime up ($peersSeen peers)").toList ++
                firstAt(_ == Bootstrapped).map(_ -> "initial stack bootstrapped").toList ++
                firstAt(_ == InitEffects).map(_ -> "init effects registered → L1").toList ++
                firstAt {
                    case EffectsLearned(s, _, _, _) => s > 0; case _ => false
                }.map(_ -> "first settlement effects → L1").toList ++
                firstAt {
                    case EffectsLearned(_, _, _, fin) => fin; case _ => false
                }.map(_ -> "finalization effects → L1").toList ++
                evs.filter(_.event.isInstanceOf[FellBack])
                    .minByOption(_.at)
                    .map(e => e.at -> s"fallback → rule-based (${txIdOf(e)})")
                    .toList ++
                firstLandedByFamily.toList
                    .map((fam, e) => e.at -> s"first $fam landed (${txIdOf(e)})") ++
                // NoMore fires repeatedly per peer ("staying alive for rollbacks"); count distinct
                // peers, take the last as the completion time.
                evacuated
                    .maxByOption(_.at)
                    .map(e =>
                        e.at -> s"evacuation complete (${evacuated.map(_.peer).distinct.size}/$peersSeen peers)"
                    )
                    .toList

        if items.isEmpty then Doc.text("(none)")
        else stack(items.sortBy(_._1).map((at, label) => line(rel(at), label)))

    /** One row per peer. Multisig columns (soft-confirms minor/major, hard stacks, stacks closed
      * leader/follower, L1 effects registered) then the fallback + rule-based columns (per-family
      * landed counts, evacuation payouts + completion).
      */
    private def peerTable(evs: Vector[Entry]): Doc =
        val families = evs.collect { case Entry(_, _, TxLanded(f, _)) => f }.distinct.sorted
        val header = List("peer", "soft m/M", "hard", "closed L/F", "L1eff", "fell-back") ++
            families ++ List("payouts", "evac")

        val rows = evs.map(_.peer).distinct.sortBy(peerOrder).map { peer =>
            val es = evs.filter(_.peer == peer).map(_.event)
            val softMinor = es.count {
                case SoftConfirmed(_, "minor", _, _) => true; case _ => false
            }
            val softMajor = es.count {
                case SoftConfirmed(_, "major", _, _) => true; case _ => false
            }
            val hard = es.count(_.isInstanceOf[StackHardConfirmed])
            val closedL = es.count { case StackClosed(_, true) => true; case _ => false }
            val closedF = es.count { case StackClosed(_, false) => true; case _ => false }
            val l1eff = es.count(_.isInstanceOf[EffectsLearned])
            val fellBack = if es.exists(_.isInstanceOf[FellBack]) then "yes" else "-"
            val landed = families
                .map(f => es.count { case TxLanded(`f`, _) => true; case _ => false }.toString)
            // First PayoutsLeft is the pre-drain total (see RbrMbtSuite.firstPayoutsLeft).
            val payouts =
                es.collectFirst { case PayoutsLeft(n) => n }.map(_.toString).getOrElse("-")
            val evac = if es.contains(Evacuated) then "done" else "-"
            List(
              peerLabel(peer),
              s"$softMinor/$softMajor",
              hard.toString,
              s"$closedL/$closedF",
              l1eff.toString,
              fellBack
            ) ++ landed ++ List(payouts, evac)
        }
        grid(header :: rows.toList)

    /** Per family: how many submissions were attempted vs how many distinct txs actually landed —
      * the collapsed race count.
      */
    private def contention(evs: Vector[Entry]): Doc =
        val families = evs
            .collect {
                case Entry(_, _, TxAttempt(f))   => f
                case Entry(_, _, TxLanded(f, _)) => f
            }
            .distinct
            .sorted
        if families.isEmpty then Doc.text("(none)")
        else
            grid(families.map { f =>
                val attempts = evs.count {
                    case Entry(_, _, TxAttempt(`f`)) => true; case _ => false
                }
                val landed = evs.collect { case Entry(_, _, TxLanded(`f`, id)) => id }.distinct.size
                List(f, s"$attempts attempts → $landed landed")
            }.toList)

    /** Distinct `(version, size)` committed maps, sorted by version — the evacuation-map size the
      * head would resolve to under each version.
      */
    private def committedMaps(evs: Vector[Entry]): Doc =
        val maps = evs
            .collect { case Entry(_, _, CommittedMap(v, s)) => (v, s) }
            .distinct
            .sortBy((v, _) => (v.major: Int, v.minor: Int))
        if maps.isEmpty then Doc.text("(none)")
        else
            grid(
              maps.map((v, s) => List(s"v${v.major: Int}.${v.minor: Int}", s"$s obligation(s)"))
                  .toList
            )

    // ------------------------------------------------------------------
    // Small paiges/formatting helpers
    // ------------------------------------------------------------------

    /** A section header with its body indented two spaces, then a trailing blank line. */
    private def section(header: Doc, body: Doc): Doc =
        header + Doc.hardLine + body.indent(2) + Doc.hardLine + Doc.hardLine

    private def stack(lines: Iterable[Doc]): Doc = Doc.intercalate(Doc.hardLine, lines)

    /** A `key … value` line with the key left-padded to a small fixed width for a hanging indent.
      */
    private def line(key: String, value: String): Doc =
        Doc.text(key.reverse.padTo(10, ' ').reverse) + Doc.text("  ") + Doc.text(value)

    /** Render a list of equal-length string rows as a column-aligned grid (two-space gutter). Any
      * header row is just the first row — it gets no special styling.
      */
    private def grid(rows: List[List[String]]): Doc =
        val widths = rows.transpose.map(col => col.map(_.length).max)
        val rendered = rows.map { row =>
            Doc.text(
              row.zip(widths).map((cell, w) => cell.padTo(w, ' ')).mkString("  ").stripTrailing
            )
        }
        stack(rendered)

    private def txIdOf(e: Entry): String = e.event match
        case FellBack(id)    => short(id)
        case TxLanded(_, id) => short(id)
        case _               => ""

    /** First 12 hex chars of a tx hash — enough to eyeball / grep the full log, not so long it
      * bloats the summary. Strips the quotes `TransactionHash.toString` wraps the hex in.
      */
    private def short(h: TransactionHash): String =
        val hex = h.toString.filter(c => c.isLetterOrDigit)
        if hex.length > 12 then hex.take(12) + "…" else hex

    private def peerLabel(p: PeerId): String = p match
        case PeerId.Head(n) => s"head-${n: Int}"
        case PeerId.Coil(n) => s"coil-${n.convert}"

    private def peerOrder(p: PeerId): (Int, Int) = p match
        case PeerId.Head(n) => (0, n: Int)
        case PeerId.Coil(n) => (1, n.convert)

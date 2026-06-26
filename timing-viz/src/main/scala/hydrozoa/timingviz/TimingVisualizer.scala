package hydrozoa.timingviz

import cats.data.State
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.timingviz.Command.*
import hydrozoa.timingviz.Ids.*
import hydrozoa.timingviz.TimedObject.*
import scala.math.Ordered.orderingToOrdered

/** A successful transition can still emit hints (e.g. snap, derivation edge). A failed transition
  * carries only the rejection — hints on the failure path are deferred until we see a need.
  */
final case class TransitionOutput[+A](
    result: Either[Rejection, A],
    hints: List[UIHint]
)

object TransitionOutput:
    def ok[A](a: A, hints: List[UIHint] = Nil): TransitionOutput[A] =
        TransitionOutput(Right(a), hints)
    def rejected(r: Rejection): TransitionOutput[Nothing] =
        TransitionOutput(Left(r), Nil)

/** The pure core: a `Command` folds into `TimingVisualizerState`, producing a `TransitionOutput`.
  * The live-streaming `Sink` layer wraps this at the edge.
  */
type Transition[A] = State[TimingVisualizerState, TransitionOutput[A]]

object TimingVisualizer:
    def apply(cmd: Command): Transition[Unit] = cmd match
        case AdvanceClock(to)                  => advanceClock(to)
        case SetParameter(key, value)          => setParameter(key, value)
        case ObserveBlock(id, creation, kind)  => addBlock(id, creation, kind, Source.Observed)
        case ObserveRequest(id, validity)      => addRequest(id, validity, Source.Observed)
        case ObserveDepositRequest(id, end)    => addDeposit(id, end, Source.Observed)
        case ObserveDepositOnChain(id)         => transitionDeposit(id, DepositStatus.OnChain)
        case ObserveDepositAbsorbed(id, by)    => transitionDeposit(id, DepositStatus.Absorbed(by))
        case ObserveDepositRejected(id, by)    => transitionDeposit(id, DepositStatus.Rejected(by))
        case ObserveSpineEffect(id, k, bid, v) => addSpineEffect(id, k, bid, v, Source.Observed)
        case ObserveFallback(id, paired, start) =>
            addFallback(id, paired, start, Source.Observed)
        case ObserveRefund(id, depId, start) =>
            addRefund(id, depId, start, Source.Observed)
        case HypothesizeDeposit(id, end) =>
            addDeposit(id, end, Source.Hypothetical)
        case HypothesizeBlock(id, creation, kind) =>
            addBlock(id, creation, kind, Source.Hypothetical)
        case Retract(id) => retract(id)

    // --- private helpers --------------------------------------------------------

    // Scrubbing intentionally allowed in both directions: "now" is a presentation cursor, not a
    // monotonic real-time clock. `Rejection.ClockWentBackwards` is kept for potential strict-mode
    // use, but `AdvanceClock` no longer emits it.
    private def advanceClock(to: QuantizedInstant): Transition[Unit] = State { s =>
        (s.copy(now = to), TransitionOutput.ok(()))
    }

    private def setParameter(
        key: ParameterKey,
        value: hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
    ): Transition[Unit] = State { s =>
        import TxTiming.Durations.*
        val newConfig = key match
            case ParameterKey.MinSettlementDuration =>
                s.config.copy(minSettlementDuration = MinSettlementDuration(value))
            case ParameterKey.InactivityMarginDuration =>
                s.config.copy(inactivityMarginDuration = InactivityMarginDuration(value))
            case ParameterKey.SilenceDuration =>
                s.config.copy(silenceDuration = SilenceDuration(value))
            case ParameterKey.DepositSubmissionDuration =>
                s.config.copy(depositSubmissionDuration = DepositSubmissionDuration(value))
            case ParameterKey.DepositMaturityDuration =>
                s.config.copy(depositMaturityDuration = DepositMaturityDuration(value))
            case ParameterKey.DepositAbsorptionDuration =>
                s.config.copy(depositAbsorptionDuration = DepositAbsorptionDuration(value))
        (s.copy(config = newConfig), TransitionOutput.ok(()))
    }

    private def addBlock(
        id: BlockId,
        creation: Interval,
        kind: BlockKind,
        source: Source
    ): Transition[Unit] =
        validateInterval(creation) match
            case Some(r) => State.pure(TransitionOutput.rejected(r))
            case None    => insertNew(Block(id, creation, kind, forced = false), source)

    private def addRequest(id: RequestId, validity: Interval, source: Source): Transition[Unit] =
        validateInterval(validity) match
            case Some(r) => State.pure(TransitionOutput.rejected(r))
            case None    => insertNew(Request(id, validity), source)

    private def addDeposit(
        id: DepositId,
        end: TxTiming.RequestTimes.RequestValidityEndTime,
        source: Source
    ): Transition[Unit] =
        insertNew(Deposit(id, end, DepositStatus.Pending), source)

    private def addSpineEffect(
        id: EffectId,
        kind: SpineKind,
        blockId: BlockId,
        validity: Interval,
        source: Source
    ): Transition[Unit] =
        validateInterval(validity) match
            case Some(r) => State.pure(TransitionOutput.rejected(r))
            case None    => insertNew(SpineEffect(id, kind, validity, blockId), source)

    private def addFallback(
        id: EffectId,
        pairedEffectId: EffectId,
        start: TxTiming.BlockTimes.FallbackTxStartTime,
        source: Source
    ): Transition[Unit] =
        insertNew(Fallback(id, start, pairedEffectId), source)

    private def addRefund(
        id: EffectId,
        depositId: DepositId,
        start: QuantizedInstant,
        source: Source
    ): Transition[Unit] =
        insertNew(Refund(id, start, depositId), source)

    private def transitionDeposit(
        id: DepositId,
        newStatus: DepositStatus
    ): Transition[Unit] = State { s =>
        val oid = ObjectId.OfDeposit(id)
        s.objects.get(oid) match
            case Some(TaggedObject(d: Deposit, src)) =>
                val updated = TaggedObject(d.copy(status = newStatus), src)
                (s.copy(objects = s.objects.updated(oid, updated)), TransitionOutput.ok(()))
            case _ =>
                (s, TransitionOutput.rejected(Rejection.UnknownObject(oid)))
    }

    private def retract(id: ObjectId): Transition[Unit] = State { s =>
        if !s.objects.contains(id) then (s, TransitionOutput.rejected(Rejection.UnknownObject(id)))
        else
            val purged = s.derivations.view
                .filterKeys((tgt, _) => tgt != id)
                .mapValues(_.filterNot((src, _) => src == id))
                .toMap
            (s.copy(objects = s.objects - id, derivations = purged), TransitionOutput.ok(()))
    }

    private def insertNew(obj: TimedObject, source: Source): Transition[Unit] = State { s =>
        val oid = obj.objectId
        if s.objects.contains(oid) then
            (s, TransitionOutput.rejected(Rejection.DuplicateObject(oid)))
        else
            (
              s.copy(objects = s.objects + (oid -> TaggedObject(obj, source))),
              TransitionOutput.ok(())
            )
    }

    private def validateInterval(i: Interval): Option[Rejection] =
        if i.start > i.end then Some(Rejection.InvalidInterval(i.start, i.end)) else None

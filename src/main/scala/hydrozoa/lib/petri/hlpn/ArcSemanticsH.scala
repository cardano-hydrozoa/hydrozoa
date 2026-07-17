package hydrozoa.lib.petri.hlpn

import scala.annotation.unused

/** Mode-relative arc semantics for HLPN (ISO 15909-1 `W(p,t)` / `W(t,p)`). Direction-neutral: the
  * arc topology records only the (place, transition) pair, and consume-vs-produce is carried here
  * by which of `pre`/`post` is non-empty. Under a binding `β` and the current place marking, the
  * arc removes `pre` and adds `post`; it is arc-locally enabled when `pre ≤ M(p)`. With the net's
  * single-arc-per-pair rule, one arc carries the full effect on its place.
  */
trait ArcSemanticsH[C]:

    /** `W(p,t)⟦β⟧`: tokens removed from the place (∅ for a pure output arc). */
    def pre(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]]

    /** `W(t,p)⟦β⟧`: tokens added to the place (∅ for a pure input arc). */
    def post(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]]

    /** Arc-local enabling (ISO Concept 23, the per-arc conjunct): every required token is present,
      * `W(p,t)⟦β⟧ ≤ M(p)`. The guard `Φ` is the other conjunct and is checked once per transition,
      * not here.
      */
    final def enabled(binding: Binding, marking: MultiSet[C]): Either[EvalError, Boolean] =
        pre(binding, marking).map(w => w.multiplicityMap.forall((c, n) => n <= marking.get(c)))

    /** The place's marking after this arc fires: `M − pre + post` (ISO Concept 24). Does not check
      * enabledness — the simulator does that first.
      */
    final def fire(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
        for
            w <- pre(binding, marking)
            z <- post(binding, marking)
        yield marking.combineWith(w)(_ - _).combineWith(z)(_ + _)

    protected final def empty(marking: MultiSet[C]): MultiSet[C] = marking.filter(_ => false)

object ArcSemanticsH:

    /** PT — consumes `W(p,t)⟦β⟧`. */
    final case class Consume[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(binding: Binding, @unused marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)
        def post(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(empty(marking))

    /** TP — produces `W(t,p)⟦β⟧`. */
    final case class Produce[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(empty(marking))
        def post(binding: Binding, @unused marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)

    /** Read — requires `W⟦β⟧` present without net change (`pre = post`). */
    final case class Read[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(binding: Binding, @unused marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)
        def post(binding: Binding, @unused marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)

    /** Reset — drains all tokens (ISO Concept 7's `A-post` placeholder). */
    final case class Reset[C]() extends ArcSemanticsH[C]:
        def pre(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(marking)
        def post(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(empty(marking))

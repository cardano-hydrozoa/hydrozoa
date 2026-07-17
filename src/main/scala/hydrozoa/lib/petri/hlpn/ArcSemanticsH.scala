package hydrozoa.lib.petri.hlpn

import scala.annotation.unused

/** Mode-relative arc semantics for HLPN (ISO 15909-1 `W(p,t)` / `W(t,p)`). Direction-neutral: the
  * arc topology records only the (place, transition) pair, and consume-vs-produce is carried here
  * by which of `pre`/`post` is non-empty. Under a binding `β` and the current place marking, the
  * arc removes `pre` and adds `post`; it is arc-locally enabled when `pre ≤ M(p)` and
  * `sideCondition` holds. With the net's single-arc-per-pair rule, one arc carries the full effect
  * on its place.
  */
trait ArcSemanticsH[C]:

    /** `W⁻(β)`: tokens removed from the place. */
    def pre(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]]

    /** `W⁺(β)`: tokens added to the place. */
    def post(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]]

    /** An enabling side-condition beyond `pre ≤ M(p)` (e.g. inhibitor: `M(p) = ∅`). */
    def sideCondition(binding: Binding, marking: MultiSet[C]): Boolean

    /** Arc-local enabling: every required token is present (`pre ≤ M(p)`) and the side-condition
      * holds.
      */
    final def enabled(binding: Binding, marking: MultiSet[C]): Either[EvalError, Boolean] =
        pre(binding, marking).map(w =>
            sideCondition(binding, marking) &&
                w.multiplicityMap.forall((c, n) => n <= marking.get(c))
        )

    /** The place's marking after this arc fires: `M − pre + post`. Does not check enabledness — the
      * simulator does that first.
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
        def sideCondition(@unused binding: Binding, @unused marking: MultiSet[C]): Boolean = true

    /** TP — produces `W(t,p)⟦β⟧`. */
    final case class Produce[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(empty(marking))
        def post(binding: Binding, @unused marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)
        def sideCondition(@unused binding: Binding, @unused marking: MultiSet[C]): Boolean = true

    /** Read — requires `W⟦β⟧` present without net change (`pre = post`). */
    final case class Read[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(binding: Binding, @unused marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)
        def post(binding: Binding, @unused marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)
        def sideCondition(@unused binding: Binding, @unused marking: MultiSet[C]): Boolean = true

    /** Inhibitor — enabled only when the place is empty. */
    final case class Inhibitor[C]() extends ArcSemanticsH[C]:
        def pre(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(empty(marking))
        def post(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(empty(marking))
        def sideCondition(@unused binding: Binding, marking: MultiSet[C]): Boolean =
            marking.multiplicityMap.isEmpty

    /** Reset — drains all tokens. */
    final case class Reset[C]() extends ArcSemanticsH[C]:
        def pre(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(marking)
        def post(@unused binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
            Right(empty(marking))
        def sideCondition(@unused binding: Binding, @unused marking: MultiSet[C]): Boolean = true

package hydrozoa.lib.petri.hlpn

import hydrozoa.lib.collection.Multiset
import scala.annotation.unused
import spire.algebra.Order

/** Mode-relative arc semantics for HLPN (ISO 15909-1 Concept 21). An arc annotation `W` is a color
  * function `C(t) → Bag(C(p))`: given a mode `β` — an element of the transition's color domain
  * `C(t)`, i.e. a binding of its variables — it yields a bag over the place's color domain `C(p)`.
  * `pre` is `W(p,t)`, `post` is `W(t,p)`; both are functions of the binding *only*, as the standard
  * requires. Direction-neutral: the arc records only the (place, transition) pair, and
  * consume-vs-produce is which of `pre`/`post` is non-empty. With the single-arc-per-pair rule, one
  * arc carries the full effect on its place.
  */
trait ArcSemanticsH[C]:

    /** `W(p,t)⟦β⟧`: the input annotation at mode `β` (∅ for a pure output arc). */
    def pre(binding: Binding): Either[EvalError, MultiSet[C]]

    /** `W(t,p)⟦β⟧`: the output annotation at mode `β` (∅ for a pure input arc). */
    def post(binding: Binding): Either[EvalError, MultiSet[C]]

    /** Arc-local enabling (ISO Concept 23, the per-arc conjunct): `W(p,t)⟦β⟧ ≤ M(p)`. The guard `Φ`
      * is the other conjunct and is checked once per transition, not here.
      */
    final def enabled(binding: Binding, marking: MultiSet[C]): Either[EvalError, Boolean] =
        pre(binding).map(w => w.multiplicityMap.forall((c, n) => n <= marking.get(c)))

    /** The place's marking after firing: `M − W(p,t) + W(t,p)` (ISO Concept 24). Does not check
      * enabledness — the simulator does that first.
      */
    final def fire(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]] =
        for
            w <- pre(binding)
            z <- post(binding)
        yield marking.combineWith(w)(_ - _).combineWith(z)(_ + _)

object ArcSemanticsH:

    /** Input arc — `W(p,t) = inscription`, `W(t,p) = ∅`. */
    final case class Consume[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(binding: Binding): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)
        def post(@unused binding: Binding): Either[EvalError, MultiSet[C]] =
            Right(emptyBag(inscription.sort))

    /** Output arc — `W(p,t) = ∅`, `W(t,p) = inscription`. */
    final case class Produce[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(@unused binding: Binding): Either[EvalError, MultiSet[C]] =
            Right(emptyBag(inscription.sort))
        def post(binding: Binding): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)

    /** A place that is both pre- and postcondition with the same annotation (`W(p,t) = W(t,p)`):
      * the tokens must be present but firing does not change the marking.
      */
    final case class Read[C](inscription: Inscription[C]) extends ArcSemanticsH[C]:
        def pre(binding: Binding): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)
        def post(binding: Binding): Either[EvalError, MultiSet[C]] =
            Binding.evalInscription(inscription, binding)

    /** The empty bag over a sort — `W = ∅` for the direction an arc does not use. */
    private def emptyBag[C](sort: Sort[C]): MultiSet[C] =
        given Order[C] = sort.order
        Multiset.empty

package hydrozoa.lib.petri.net

/** The token algebra a net type instantiates the ISO 15909-1 rules over: `W` is the arc-annotation
  * type, `M` the place-marking type. The standard states enabling and firing once, generically
  * (Concepts 6/7), and each net type re-instantiates the same formulas at its own token types
  * (Concepts 10/12 for P/T over ℕ; Concepts 23/24 for HLPN over multisets). This trait captures
  * exactly what varies between those instantiations, so one simulator serves every net type.
  *
  * Firing an enabled transition cannot fail in ISO (Concept 7 has no failure path), so `minus` and
  * `plus` are total: `minus` under the precondition `covers(m, w)` — which the simulator checks
  * first — and `plus` unconditionally (markings are unbounded). A `minus` precondition violation is
  * a bug and should fail loudly, not return an error.
  */
trait MarkingAlgebra[W, M] {

    /** The enabling test `M(p) ≥ W(p,t)` (Concept 10). */
    def covers(m: M, w: W): Boolean

    /** `M(p) − W(p,t)` (Concept 12). Precondition: `covers(m, w)`. */
    def minus(m: M, w: W): M

    /** `M(p) + W(t,p)` (Concept 12). Total — markings are unbounded. */
    def plus(m: M, w: W): M
}

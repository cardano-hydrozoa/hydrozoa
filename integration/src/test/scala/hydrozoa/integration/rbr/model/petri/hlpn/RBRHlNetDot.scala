package hydrozoa.integration.rbr.model.petri.hlpn

import hydrozoa.lib.petri.hlpn.*
import hydrozoa.lib.petri.net.components.Arc
import spire.math.SafeLong

/** A quick-and-dirty Graphviz DOT rendering of an [[HlNet]], for eyeballing structure. Places are
  * ellipses (id + current token count, filled when marked), transitions are boxes (id + guard), and
  * every `Arc.Flow` is a directed edge labelled with its inscription. [[toDot]] draws the whole net;
  * [[toDotPerTransition]] draws one small diagram per transition (the whole net is too dense to
  * read). Render with e.g. `dot -Tsvg target/rbr-net/Vote.dot -o Vote.svg`, or `just graphviz`.
  */
object RBRHlNetDot {

    /** The whole net as one DOT `digraph` — every place, transition, and arc. Dense for a net this
      * size; see [[toDotPerTransition]] for a readable split.
      */
    def toDot[P, T, C](net: HlNet[P, T, C]): String =
        wrap(
          "RBR",
          net.placesMap.toList.map((id, p) => renderPlace(id, p)) ++
              net.transitionsMap.toList.map((id, t) => renderTransition(id, t)) ++
              net.arcsMap.toList.map((flow, arc) => renderEdge(flow, arc))
        )

    /** One DOT `digraph` per transition — just that transition, the places it connects to, and its
      * arcs. Splits the dense whole-net view into a diagram small enough to eyeball per transition.
      */
    def toDotPerTransition[P, T, C](net: HlNet[P, T, C]): List[(T, String)] =
        net.transitionsMap.toList.map { (tid, t) =>
            val arcs = net.arcsMap.toList.filter((flow, _) => flow.transition == tid)
            val placeNodes = arcs
                .map((flow, _) => flow.place)
                .distinct
                .flatMap(pid => net.placesMap.get(pid).map(p => renderPlace(pid, p)))
            val body = (renderTransition(tid, t) :: placeNodes) ++ arcs.map((flow, a) => renderEdge(flow, a))
            tid -> wrap(tid.toString, body)
        }

    private def wrap(name: String, body: List[String]): String =
        (List(
          s"""digraph "${esc(name)}" {""",
          "  rankdir=LR;",
          """  node [fontname="monospace" fontsize=10];""",
          """  edge [fontname="monospace" fontsize=9];"""
        ) ++ body ++ List("}")).mkString("\n")

    private def renderPlace[P, C](id: P, place: ColoredPlace[C]): String = {
        val count = place.marking.multiplicityMap.values.foldLeft(SafeLong(0))(_ + _)
        val fill = if count > SafeLong(0) then """style=filled fillcolor="#cce5ff" """ else ""
        s"""  "P_$id" [shape=ellipse $fill label="${esc(id.toString)}$nl($count)"];"""
    }

    private def renderTransition[T, C](id: T, t: HlTransition[C]): String = {
        val guard = renderGuard(t.guard)
        val label =
            if guard.isEmpty then esc(id.toString) else s"${esc(id.toString)}$nl[${esc(guard)}]"
        s"""  "T_$id" [shape=box style=filled fillcolor="#eeeeee" label="$label"];"""
    }

    private def renderEdge[P, T](flow: Arc.Flow[P, T], arc: InscribedArc[?]): String = {
        val w = esc(renderInscription(arc.inscription))
        flow match
            case Arc.Flow.Pt(p, t) => s"""  "P_$p" -> "T_$t" [label="$w"];"""
            case Arc.Flow.Tp(t, p) => s"""  "T_$t" -> "P_$p" [label="$w"];"""
    }

    /** A line break inside a DOT label (literal backslash-n in the emitted text). */
    private val nl = "\\n"

    private def esc(s: String): String = s.replace("\\", "\\\\").replace("\"", "\\\"")

    private def renderInscription(insc: Inscription[?]): String =
        insc match
            case Inscription.Weighted(coeff, color) =>
                if coeff.toInt == 1 then renderColor(color)
                else s"${coeff.toInt}·${renderColor(color)}"
            case Inscription.Union(l, r) => s"${renderInscription(l)} ⊕ ${renderInscription(r)}"
            case Inscription.Collect(cv, pattern) =>
                s"⟦${cv.name}≤${cv.bound}: ${renderColor(pattern)}⟧"
            case Inscription.Inhibit(pattern) => s"○ ${renderColor(pattern)}"

    private def renderColor(term: ColorTerm[?]): String =
        term match
            case ColorTerm.Ref(v) => v.name
            case ColorTerm.Const(value, _) =>
                value match
                    case _: Unit => "•"
                    case other   => other.toString
            case ColorTerm.Succ(inner) => s"${renderColor(inner)}++"
            case ColorTerm.Tuple(l, r) => s"(${renderColor(l)},${renderColor(r)})"
            case ColorTerm.Wildcard(_) => "*"

    private def renderGuard(guard: Guard): String =
        guard match
            case Guard.True             => ""
            case Guard.Eq(l, r)         => s"${renderColor(l)}=${renderColor(r)}"
            case Guard.Lt(l, r)         => s"${renderColor(l)}<${renderColor(r)}"
            case Guard.InSubclass(c, s) => s"${renderColor(c)}∈$s"
            case Guard.Not(g)           => s"¬(${renderGuard(g)})"
            case Guard.And(l, r)        => s"(${renderGuard(l)} ∧ ${renderGuard(r)})"
            case Guard.Or(l, r)         => s"(${renderGuard(l)} ∨ ${renderGuard(r)})"
}

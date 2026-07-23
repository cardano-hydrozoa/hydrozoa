package hydrozoa.lib.petri.hlpn

import hydrozoa.lib.petri.net.components.Place
import spire.math.SafeLong

/** An HLPN place (ISO 15909-1 `C(p)`): its marking is a [[MultiSet]] over the colors of a declared
  * color domain. It slots into the existing Place metamodel unchanged —
  * [[Place.Syntax.PlaceMarking]] is simply fixed to a multiset rather than a token count, and the
  * `mark` / `markingError` / `validMarking` machinery applies as-is.
  *
  * A marking is valid iff every color it carries belongs to `colorDomain` and every multiplicity is
  * non-negative. The non-negativity check is where "a marking is a non-negative multiset" is
  * enforced (E1), since the [[MultiSet]] carrier is a signed integer to give firing a group.
  */
abstract class ColoredPlace[C]
    extends Place.Topology,
      Place.Syntax[ColoredPlace[C]],
      Place.Semantics[ColoredPlace[C]]:
    final type PlaceMarking = MultiSet[C]

    /** `C(p)`: the color domain this place's tokens are drawn from. */
    def colorDomain: Sort[C]

    override def markingError: Option[Place.Semantics.MarkingError] =
        marking.multiplicityMap.collectFirst {
            case (c, _) if !colorDomain.contains(c) => ColoredPlace.OutOfDomain(c.toString)
            case (c, n) if n < SafeLong.zero => ColoredPlace.NegativeMultiplicity(c.toString, n)
        }

object ColoredPlace:

    /** A marking carries a color outside the place's declared color domain. */
    final case class OutOfDomain(color: String) extends Place.Semantics.MarkingError:
        override def getMessage: String = s"Color $color is not in the place's color domain"

    /** A marking carries a negative multiplicity — an HLPN marking is a non-negative multiset. */
    final case class NegativeMultiplicity(color: String, count: SafeLong)
        extends Place.Semantics.MarkingError:
        override def getMessage: String = s"Color $color has negative multiplicity $count"

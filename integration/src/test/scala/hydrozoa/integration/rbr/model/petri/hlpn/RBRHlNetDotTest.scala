package hydrozoa.integration.rbr.model.petri.hlpn

import java.nio.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite

/** Renders the RBR net to `target/rbr-net.dot` (then `dot -Tpng target/rbr-net.dot -o rbr.png`) and
  * sanity-checks that the DOT names every place and transition.
  */
class RBRHlNetDotTest extends AnyFunSuite:

    test("RBRHlNetDot renders the RBR net to target/rbr-net.dot") {
        val net = RBRHlNet(nHeadPeers = 3, maxVersionMinor = 2).toOption.get
        val dot = RBRHlNetDot.toDot(net)
        val out = Path.of("target", "rbr-net.dot")
        Files.createDirectories(out.toAbsolutePath.getParent)
        val _ = Files.writeString(out, dot)
        val _ = assert(dot.startsWith("digraph"))
        val _ = assert(net.placesMap.keySet.forall(id => dot.contains(id.toString)))
        val _ = assert(net.transitionsMap.keySet.forall(id => dot.contains(id.toString)))
        assert(dot.contains("batch")) // the Evacuation collection arc rendered
    }

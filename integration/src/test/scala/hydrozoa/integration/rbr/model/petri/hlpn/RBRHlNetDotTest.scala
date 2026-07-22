package hydrozoa.integration.rbr.model.petri.hlpn

import java.nio.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite

/** Renders the RBR net as DOT — the whole net to `target/rbr-net.dot` and one diagram per
  * transition under `target/rbr-net/` (see `just graphviz`) — and sanity-checks the output.
  */
class RBRHlNetDotTest extends AnyFunSuite:

    test("RBRHlNetDot renders the RBR net, whole and per transition") {
        val net = RBRHlNet(nHeadPeers = 3, maxVersionMinor = 2).toOption.get

        // whole net (dense) — one file
        val whole = Path.of("target", "rbr-net.dot")
        Files.createDirectories(whole.toAbsolutePath.getParent)
        val _ = Files.writeString(whole, RBRHlNetDot.toDot(net))

        // one diagram per transition — the readable split
        val dir = Path.of("target", "rbr-net")
        Files.createDirectories(dir.toAbsolutePath)
        val perTransition = RBRHlNetDot.toDotPerTransition(net)
        perTransition.foreach { (tid, dot) =>
            val _ = Files.writeString(dir.resolve(s"$tid.dot"), dot)
        }

        // one valid digraph per transition, each naming its own transition
        val _ = assert(perTransition.map(_._1).toSet == net.transitionsMap.keySet)
        val _ = assert(perTransition.forall((tid, dot) => dot.startsWith("digraph") && dot.contains(tid.toString)))
        // the ISO 15909-3 enrichments render on their transitions: Evacuation's batch (collection),
        // Deinit's inhibitor (§A.4 circle head), and read arcs (§A.5 double-headed)
        val _ = assert(perTransition.exists((tid, dot) => tid.toString == "Evacuation" && dot.contains("batch")))
        val _ = assert(perTransition.exists((tid, dot) => tid.toString == "Deinit" && dot.contains("arrowhead=odot")))
        assert(perTransition.exists((_, dot) => dot.contains("dir=both")))
    }

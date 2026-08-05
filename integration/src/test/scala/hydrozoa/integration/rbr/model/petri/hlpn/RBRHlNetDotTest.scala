package hydrozoa.integration.rbr.model.petri.hlpn

import java.nio.file.{Files, Path}
import org.scalacheck.{Prop, Properties}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, TransactionOutput, Value}

/** Renders the RBR net as DOT — the whole net to `target/rbr-net.dot` and one diagram per
  * transition under `target/rbr-net/` (see `just graphviz`) — and sanity-checks the output.
  */
object RBRHlNetDotTest extends Properties("RBRHlNetDot"):

    private val payoutAddress: Address =
        Address.fromBech32("addr_test1wqt2v8zcpjldyu2zcwz3yuu8p4wpk0hzaqwthh23qgs5xgg7266qn")

    // A representative seed: versions 1 and 2, version 2 carrying a two-output evacuation batch.
    private val committedObligations: Map[BigInt, List[TransactionOutput]] =
        List(1, 2).map { v =>
            BigInt(v) -> (1 to v).toList.map { j =>
                TransactionOutput(
                  payoutAddress,
                  Value(Coin((v.toLong * 100 + j) * 1_000_000L))
                )
            }
        }.toMap

    val _ = property("RBRHlNetDot renders the RBR net, whole and per transition") = {
        val net =
            RBRHlNet(nHeadPeers = 3, committedObligations).toOption.get

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

        Prop.all(
          // one valid digraph per transition, each naming its own transition
          Prop.propBoolean(perTransition.map(_._1).toSet == net.transitionsMap.keySet),
          Prop.propBoolean(
            perTransition.forall((tid, dot) =>
                dot.startsWith("digraph") && dot.contains(tid.toString)
            )
          ),
          // the ISO 15909-3 enrichments render on their transitions: Evacuation's batch (collection),
          // Deinit's inhibitor (§A.4 circle head), and read arcs (§A.5 plain undirected segment)
          Prop.propBoolean(
            perTransition.exists((tid, dot) =>
                tid.toString == "Evacuation" && dot.contains("batch")
            )
          ),
          Prop.propBoolean(
            perTransition.exists((tid, dot) =>
                tid.toString == "Deinit" && dot.contains("arrowhead=odot")
            )
          ),
          Prop.propBoolean(perTransition.exists((_, dot) => dot.contains("dir=none")))
        )
    }

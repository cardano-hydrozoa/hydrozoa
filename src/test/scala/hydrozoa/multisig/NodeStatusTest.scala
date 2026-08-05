package hydrozoa.multisig

import hydrozoa.multisig.NodeStatus.advanceTo
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

/** Properties of the monotone [[NodeStatus.advanceTo]] lifecycle advance. With four statuses the
  * generators cover the full 4×4 transition table, so these properties characterize the function
  * exhaustively.
  */
object NodeStatusTest extends Properties("NodeStatus") {

    private val genStatus: Gen[NodeStatus] = Gen.oneOf(NodeStatus.values.toSeq)

    private val genTerminal: Gen[NodeStatus] =
        Gen.oneOf(NodeStatus.Finalized, NodeStatus.HandedOffToRuleBased)

    val _ = property("advanceTo yields one of its operands") = forAll(genStatus, genStatus) {
        (current, next) =>
            val advanced = current.advanceTo(next)
            advanced == current || advanced == next
    }

    val _ = property("advanceTo is idempotent") = forAll(genStatus) { status =>
        status.advanceTo(status) == status
    }

    val _ = property("Initializing advances to anything") = forAll(genStatus) { next =>
        NodeStatus.Initializing.advanceTo(next) == next
    }

    val _ = property("nothing regresses to Initializing") = forAll(genStatus) { current =>
        current.advanceTo(NodeStatus.Initializing) == current
    }

    val _ = property("Active advances to any terminal status") = forAll(genTerminal) { terminal =>
        NodeStatus.Active.advanceTo(terminal) == terminal
    }

    val _ = property("terminal statuses absorb every write") = forAll(genTerminal, genStatus) {
        (terminal, next) =>
            terminal.advanceTo(next) == terminal
    }
}

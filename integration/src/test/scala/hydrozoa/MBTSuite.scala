package hydrozoa

import hydrozoa.node.server.Node
import hydrozoa.node.state.HeadStateGlobal
import org.scalacheck.{Gen, Properties}
import org.scalacheck.commands.Commands

object MBTSuite extends Commands:
    // We need a wrapper around HeadState that will maintain additional state for L1
    override type State = HeadStateGlobal
    override type Sut = Node

    override def canCreateNewSut(
        newState: State,
        initSuts: Traversable[State],
        runningSuts: Traversable[Sut]
    ): Boolean = initSuts.isEmpty && runningSuts.isEmpty

    override def newSut(state: State): Sut =
        mkSimpleHydrozoaNode(Utils.protocolParams, Some(state))._2

    override def destroySut(_sut: Sut): Unit = () // TODO: shall we do something here?

    override def initialPreCondition(state: State): Boolean = ???

    override def genInitialState: Gen[State] =
        ///Gen.
        ???

    override def genCommand(state: State): Gen[Command] = ???

object SomeProperty extends Properties("Some property") {
    property("check") = MBTSuite.property()
}
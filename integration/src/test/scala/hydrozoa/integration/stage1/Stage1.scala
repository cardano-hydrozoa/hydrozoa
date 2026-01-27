package hydrozoa.integration.stage1

import org.scalacheck.commands.YetAnotherCommands
import org.scalacheck.{Gen, YetAnotherProperties}

/** Stage 1 (the simplest).
  *   - Only two actors are involved: [[JointLedger]] and [[CardanoLiaison]]
  */

object Stage1 extends YetAnotherCommands {

    override type State = this.type
    override type Sut = this.type

    override def canCreateNewSut(
        newState: Stage1.this.type,
        initSuts: Traversable[Stage1.this.type],
        runningSuts: Traversable[Stage1.this.type]
    ): Boolean = ???

    override def newSut(state: Stage1.this.type): Stage1.this.type = ???

    override def destroySut(sut: Stage1.this.type): Unit = ???

    override def initialPreCondition(state: Stage1.this.type): Boolean = ???

    override def genInitialState: Gen[Stage1.this.type] = ???

    override def genCommand(state: Stage1.this.type): Gen[Command0] = ???
}

object HydrozoaScenario1WithL1Mock
    extends YetAnotherProperties("Joint ledger and Cardano liaison (stage 1"):

    val _ = property("Work well on L1 mock (fast, reproducible)") = Stage1.property0()
    // val _ = property("Work well on Yaci DevKit (slow, reproducible)") = ???
    // val _ = property("Work well on Preview (slow, non-reproducible)") = ???

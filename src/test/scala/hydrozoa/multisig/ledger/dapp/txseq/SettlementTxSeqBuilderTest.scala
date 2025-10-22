package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.multisig.ledger.dapp.tx.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SettlementTxSeqBuilderTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 1)

    test("Build settlement tx sequence") {
        // TODO: I guess this generator doesn't work well
        forAll(genSettlementTxSeqBuilder()) { (builder,args) =>
            builder.build(args) match {
                case Left(e)  => fail(s"Build failed $e")
                case Right(_) => ()
            }
        }
    }

}

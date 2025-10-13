//package hydrozoa.multisig.ledger.dapp.txseq
//
//import hydrozoa.multisig.ledger.dapp.tx.genSettlementRecipe
//import munit.FunSuite
//import org.scalacheck.{Prop, Test as ScalaCheckTest}
//
//class SettlementTxSeqBuilderTest extends munit.ScalaCheckSuite {
//
//    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
//        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(1)
//    }
//
//    property("Build settlement tx sequence")(
//      // TODO: I guess this generator doesn't work well
//      Prop.forAll(genSettlementRecipe()) { recipe =>
//          SettlementTxSeqBuilder.build(recipe) match {
//              case Left(e)    => throw RuntimeException(s"Build failed $e")
//              case Right(ret) => println(ret)
//          }
//      }
//    )
//
//}

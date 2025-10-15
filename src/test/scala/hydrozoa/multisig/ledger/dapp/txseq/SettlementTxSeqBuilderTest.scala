package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import munit.FunSuite
import org.scalacheck.{Arbitrary, Prop, Test as ScalaCheckTest}
import test.*
import scalus.cardano.ledger.{ArbitraryInstances, *}
import scalus.cardano.ledger.ArbitraryInstances.{*, given}

import scala.collection.immutable.Queue

class SettlementTxSeqBuilderTest extends munit.ScalaCheckSuite {

    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(1000)
    }
    
    property("Build settlement tx sequence")(
      // TODO: I guess this generator doesn't work well
      Prop.forAll(genSettlementTxSeqBuilder()) { builder =>
        builder.build() match {
            case Left(e) => throw RuntimeException(s"Build failed $e")
            case Right(_) => ()
        }
      }
    )

}

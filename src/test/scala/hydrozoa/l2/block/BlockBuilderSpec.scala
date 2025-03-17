package hydrozoa.l2.block

import hydrozoa.{L1, OutputRefL1, TxId, TxIx}
import org.scalatest.Assertions.assertTypeError;

class BlockBuilderSpec extends munit.ScalaCheckSuite {

    test("no blocks without block number") {
        assertTypeError("SafeBlockBuilder().build;")
        assertTypeError("SafeBlockBuilder().majorBlock.build;")
        assertTypeError("SafeBlockBuilder().finalBlock.build;")
    }

    test("no block without major version") {
        assertTypeError("SafeBlockBuilder().blockNum(42).build;")
        assertTypeError("SafeBlockBuilder().blockNum(42).majorBlock.build;")
        assertTypeError("SafeBlockBuilder().blockNum(42).finalBlock.build;")
    }

    test("no minor versions for major and final blocks") {
        assertTypeError("SafeBlockBuilder().majorBlock.versionMinor(1)")
        assertTypeError("SafeBlockBuilder().finalBlock.versionMinor(1)")
    }

    test("minor block") {
        val minorBlock =
            BlockBuilder().blockNum(42).versionMajor(5).withTransaction(TxId("tx1hash")).build;
        println(minorBlock)
    }

    test("no minor blocks with withdrawals") {
        assertTypeError(
          "SafeBlockBuilder().blockNum(42).versionMajor(5).withWithdrawal(TxId(\"wd1hash\")).build"
        );
    }

    test("major with withdrawals") {
        println(
          BlockBuilder().majorBlock
              .blockNum(42)
              .versionMajor(5)
              .withWithdrawal(TxId("wd1hash"))
              .build
        );
    }

    test("final with withdrawals") {
        println(
          BlockBuilder().finalBlock
              .blockNum(42)
              .versionMajor(5)
              .withWithdrawal(TxId("wd1hash"))
              .build
        );
    }

    test("no minor blocks with deposits") {
        assertTypeError(
          "SafeBlockBuilder().blockNum(42).versionMajor(5).withWithdrawal(TxId(\"tx1hash\")).build"
        );
    }

    test("no final blocks with deposits") {
        assertTypeError("""SafeBlockBuilder().finalBlock
              .blockNum(42)
              .versionMajor(5)
              .withDeposits(Set(mkOutputRef[L1](TxId("wd1hash"), TxIx(0))))
              .build""")
    }

    test("major with deposits") {
        println(
          BlockBuilder().majorBlock
              .blockNum(42)
              .versionMajor(5)
              .withDeposit(OutputRefL1(TxId("wd1hash"), TxIx(0)))
              .build
        );
    }

    test("no minor blocks with withdrawals") {
        assertTypeError(
          "SafeBlockBuilder().blockNum(42).versionMajor(5).withWithdrawal(TxId(\"tx1\")).build"
        );
    }

    test("major blocks") {
        val minorBlock1 = BlockBuilder().blockNum(42).versionMajor(5).majorBlock.build;
        println(minorBlock1)

        val minorBlock2 = BlockBuilder().majorBlock.blockNum(42).versionMajor(5).build;
        println(minorBlock2)
    }

    test("final block") {
        val minorBlock = BlockBuilder().finalBlock.blockNum(42).versionMajor(5).build;
        println(minorBlock)
    }

    test("major can't be promoted as final") {
        assertTypeError("SafeBlockBuilder().majorBlock.finalBlock")
    }

}

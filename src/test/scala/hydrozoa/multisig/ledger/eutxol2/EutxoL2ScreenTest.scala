package hydrozoa.multisig.ledger.eutxol2

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis}
import hydrozoa.multisig.ledger.l2.{Destination, L2LedgerCommand}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, TransactionHash, TransactionInput, Value}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genGenesisObligation

/** EUTXO stateless screening (docs/l2-isomorphism.md): a transaction payload must parse (and carry
  * the headId pin, tested separately in [[HeadIdPinTest]]); a deposit's l2Payload must decode to
  * GenesisObligations whose total value is covered by depositL2Value. The accept path for
  * well-formed transactions is covered by the stage integration suites; deposit L1 screening (the
  * l2Payload pin + accept-by) is Hydrozoa-side and out of the ledger's scope.
  */
class EutxoL2ScreenTest extends AnyFunSuite:

    private val multiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private val nodeConfig = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    private val ledger = EutxoL2Ledger(nodeConfig).unsafeRunSync()

    private val garbage = ByteString.fromArray(Array[Byte](1, 2, 3))

    private val depositorAddress = multiNodeConfig.addressOf(HeadPeerNumber.zero)

    // One 5-ADA opening obligation; the deposit spawns exactly this on absorption.
    private val obligation = genGenesisObligation(
      depositorAddress,
      genValue = Gen.const(Value.ada(5))
    )(using nodeConfig).pureApply(Gen.Parameters.default, Seed(1L))

    private val l2Payload = GenesisObligation.serialize(NonEmptyList.one(obligation))

    private def mkScreenDeposit(
        depositL2Value: Value,
        payload: ByteString = l2Payload
    ): L2LedgerCommand.ScreenDeposit =
        L2LedgerCommand.ScreenDeposit(
          depositId = TransactionInput(
            L2Genesis.mkGenesisId(
              TransactionInput(
                TransactionHash.fromByteString(
                  blake2b_256(ByteString.fromString("EutxoL2ScreenTest"))
                ),
                0
              )
            ),
            0
          ),
          depositFee = Coin.zero,
          depositL2Value = depositL2Value,
          refundDestination = Destination(depositorAddress, None),
          l2Payload = payload
        )

    test("sendScreenTx rejects a malformed transaction payload (No)") {
        assert(ledger.sendScreenTx(garbage).value.unsafeRunSync().isLeft)
    }

    test("sendScreenDeposit accepts when depositL2Value covers the l2Payload outputs (Yes)") {
        assert(
          ledger.sendScreenDeposit(mkScreenDeposit(Value.ada(5))).value.unsafeRunSync().isRight
        )
    }

    test("sendScreenDeposit rejects when the l2Payload outputs exceed depositL2Value (No)") {
        assert(
          ledger.sendScreenDeposit(mkScreenDeposit(Value.ada(4))).value.unsafeRunSync().isLeft
        )
    }

    test("sendScreenDeposit rejects a malformed l2Payload (No)") {
        assert(
          ledger
              .sendScreenDeposit(mkScreenDeposit(Value.ada(5), payload = garbage))
              .value
              .unsafeRunSync()
              .isLeft
        )
    }

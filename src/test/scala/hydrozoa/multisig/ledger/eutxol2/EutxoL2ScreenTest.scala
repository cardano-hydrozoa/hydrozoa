package hydrozoa.multisig.ledger.eutxol2

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.{Destination, L2CommandNumber, L2LedgerCommand, L2LedgerResponse}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, TransactionHash, TransactionInput, Value}
import scalus.cardano.onchain.plutus.prelude.Option as SOption
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

    private val ledger = InMemoryL2Store.ledger(nodeConfig).unsafeRunSync()

    private val garbage = ByteString.fromArray(Array[Byte](1, 2, 3))

    private val depositorAddress = multiNodeConfig.addressOf(HeadPeerNumber.zero)

    // One 5-ADA opening obligation; the deposit spawns exactly this on absorption.
    private val obligation = genGenesisObligation(
      depositorAddress,
      genValue = Gen.const(Value.ada(5))
    )(using nodeConfig).pureApply(Gen.Parameters.default, Seed(1L))

    private val l2Payload = GenesisObligation.serialize(NonEmptyList.one(obligation))

    // A single spawned output far below min-ada (0.1 ADA), built directly — the generator
    // force-bumps to min-ada, so it cannot produce one. Its aggregate value is still "covered" by
    // an equal depositL2Value, so only the per-output min-ada gate can reject it.
    private val subMinAdaValue = Value.lovelace(100000L)

    private val subMinAdaObligation = GenesisObligation(
      l2OutputPaymentAddress = depositorAddress.payment,
      l2OutputNetwork = depositorAddress.network,
      l2OutputDatum = SOption.None,
      l2OutputValue = subMinAdaValue,
      l2OutputRefScript = None
    )

    private val subMinAdaPayload =
        GenesisObligation.serialize(NonEmptyList.one(subMinAdaObligation))

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

    private def mkRegisterDeposit(
        depositL2Value: Value,
        payload: ByteString
    ): L2LedgerCommand.RegisterDeposit =
        L2LedgerCommand.RegisterDeposit(
          requestId = RequestId(0, 0L),
          blockNumber = BlockNumber(1),
          blockCreationStartTime = BigInt(0),
          depositId = TransactionInput(
            TransactionHash.fromByteString(
              blake2b_256(ByteString.fromString("EutxoL2ScreenTest-reg"))
            ),
            0
          ),
          depositFee = Coin.zero,
          depositL2Value = depositL2Value,
          refundDestination = Destination(depositorAddress, None),
          l2Payload = payload
        )

    test("screenTx rejects a malformed transaction payload (No)") {
        assert(ledger.screenTx(garbage).value.unsafeRunSync().isLeft)
    }

    test("screenDeposit accepts when depositL2Value covers the l2Payload outputs (Yes)") {
        assert(
          ledger.screenDeposit(mkScreenDeposit(Value.ada(5))).value.unsafeRunSync().isRight
        )
    }

    test("screenDeposit rejects when the l2Payload outputs exceed depositL2Value (No)") {
        assert(
          ledger.screenDeposit(mkScreenDeposit(Value.ada(4))).value.unsafeRunSync().isLeft
        )
    }

    test(
      "screenDeposit rejects a spawned output below min-ada even when depositL2Value covers it (No)"
    ) {
        assert(
          ledger
              .screenDeposit(mkScreenDeposit(subMinAdaValue, payload = subMinAdaPayload))
              .value
              .unsafeRunSync()
              .isLeft
        )
    }

    // A fresh ledger per test: registerDeposit goes through the command-number path, and a
    // reject advances the tip — so a shared ledger would make later registrations out-of-order.
    private def freshLedger: EutxoL2Ledger = InMemoryL2Store.ledger(nodeConfig).unsafeRunSync()

    test("registerDeposit rejects a spawned output below min-ada at registration (No)") {
        assert(
          freshLedger
              .registerDeposit(
                L2CommandNumber(1L),
                mkRegisterDeposit(subMinAdaValue, subMinAdaPayload)
              )
              .unsafeRunSync()
              .isInstanceOf[L2LedgerResponse.Rejected]
        )
    }

    test(
      "registerDeposit rejects when spawned outputs exceed depositL2Value (cover, at registration)"
    ) {
        // l2Payload spawns 5 ADA; a 4-ADA deposit does not cover it, so registration must reject —
        // cover is authoritative at registration, not only at the (origin-peer, stub-for-remote) screen.
        assert(
          freshLedger
              .registerDeposit(L2CommandNumber(1L), mkRegisterDeposit(Value.ada(4), l2Payload))
              .unsafeRunSync()
              .isInstanceOf[L2LedgerResponse.Rejected]
        )
    }

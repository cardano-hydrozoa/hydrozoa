package hydrozoa.multisig.ledger.remote

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey, EvacuationMapHash}
import hydrozoa.multisig.ledger.l2.L2LedgerResponse.UnrecoverableError
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerResponse}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, RestoreResponse}
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** Round-trip tests for the `L2LedgerResponse` wire codec: every branch carries the command number,
  * and `Applied` has a concrete descendant per command (RegisterDeposit → none,
  * ApplyDepositDecisions → diffs, ApplyTransaction → diffs + payouts).
  */
class RemoteL2LedgerCodecsTest extends AnyFunSuite:

    private given CardanoNetwork.Section =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    import RemoteL2LedgerCodecs.given

    private def roundTrips(r: L2LedgerResponse): Boolean =
        io.circe.parser.decode[L2LedgerResponse](r.asJson.noSpaces) == Right(r)

    private val diff: EvacuationDiff =
        EvacuationDiff.Delete(EvacuationKey(ByteString.fromArray(Array.fill[Byte](36)(1))).get)

    test("Applied.RegisterDeposit (no effects) round-trips and carries no diff/payout fields") {
        val r = L2LedgerResponse.Applied.RegisterDeposit(L2CommandNumber(7L))
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && !json.contains("evacuationDiffs") && !json.contains("payouts"))
    }

    test("Applied.ApplyDepositDecisions (diffs, no payouts) round-trips") {
        val r = L2LedgerResponse.Applied.ApplyDepositDecisions(L2CommandNumber(8L), Vector(diff))
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && json.contains("evacuationDiffs") && !json.contains("payouts"))
    }

    test("Applied.ApplyTransaction (diffs + payouts) round-trips") {
        val r = L2LedgerResponse.Applied.ApplyTransaction(
          L2CommandNumber(9L),
          Vector(diff),
          Vector.empty
        )
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && json.contains("evacuationDiffs") && json.contains("payouts"))
    }

    test("UnrecoverableError.OutOfOrder carries the sent and the expected command number") {
        assert(roundTrips(UnrecoverableError.OutOfOrder(L2CommandNumber(5L), L2CommandNumber(4L))))
    }

    test("UnrecoverableError.LedgerFreeze carries the command number and the freezing number") {
        assert(
          roundTrips(UnrecoverableError.LedgerFreeze(L2CommandNumber(9L), L2CommandNumber(4L)))
        )
    }

    test("Rejected.RegisterDeposit (string reason) round-trips") {
        assert(
          roundTrips(L2LedgerResponse.Rejected.RegisterDeposit(L2CommandNumber(2L), "bad deposit"))
        )
    }

    test("Rejected.ApplyTransaction (string reason) round-trips") {
        assert(
          roundTrips(L2LedgerResponse.Rejected.ApplyTransaction(L2CommandNumber(3L), "bad tx"))
        )
    }

    test("UnrecoverableError.CompartmentsNotFound (missing request ids) round-trips") {
        assert(
          roundTrips(
            UnrecoverableError.CompartmentsNotFound(
              L2CommandNumber(4L),
              List(RequestId(0, 9L), RequestId(1, 3L))
            )
          )
        )
    }

    test("UnrecoverableError.OtherError (string reason) round-trips") {
        assert(
          roundTrips(UnrecoverableError.OtherError(L2CommandNumber(5L), "merge failed"))
        )
    }

    // Golden wire-shape tests pinning the exact SugarRush #140 JSON for the three restoreTo frames —
    // the cross-repo contract with the SugarRush ledger. Each asserts both the encoded string equals
    // the canonical JSON and the JSON decodes back to the original value.

    test("RestoreTo request encodes to the canonical SugarRush wire shape and round-trips") {
        val request: Request = Request.Restore(L2CommandNumber(7L))
        val json = request.asJson.noSpaces
        assert(
          json == """{"RestoreTo":{"commandNumber":7}}"""
              && io.circe.parser.decode[Request](json) == Right(request)
        )
    }

    test("Restored success encodes to the canonical SugarRush wire shape and round-trips") {
        // The same vector SugarRush pins in `types/src/types/coordination/restore.rs`.
        val hash = EvacuationMapHash(ByteString.fromArray(Array.fill[Byte](32)(0xab.toByte)))
        val response: RestoreResponse = RestoreResponse.Restored(L2CommandNumber(7L), hash, None)
        val json = response.asJson.noSpaces
        assert(
          json == """{"Restored":{"tip":7,"evacuationMapHash":"abababababababababababababababababababababababababababababababab"}}"""
              && io.circe.parser.decode[RestoreResponse](json) == Right(response)
        )
    }

    test("RestoreFailed failure encodes to the canonical SugarRush wire shape and round-trips") {
        val response: RestoreResponse =
            RestoreResponse.RestoreFailed(L2CommandNumber(9L), L2CommandNumber(4L), "tip too low")
        val json = response.asJson.noSpaces
        assert(
          json == """{"RestoreFailed":{"requested":9,"tip":4,"reason":"tip too low"}}"""
              && io.circe.parser.decode[RestoreResponse](json) == Right(response)
        )
    }

    test(
      "RegisterDeposit encodes to the golden wire form SugarRush pins (register_deposit_golden)"
    ) {
        import hydrozoa.multisig.consensus.peer.HeadPeerNumber
        import hydrozoa.multisig.ledger.block.BlockNumber
        import hydrozoa.multisig.ledger.l2.{Destination, L2LedgerCommand}
        import scalus.cardano.address.Address
        import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, HashPurpose, TransactionInput, Value}

        val refundAddress =
            "addr1z9ryamhgnuz6lau86sqytte2gz5rlktv2yce05e0h3207qkwuyc0kwgcsnu6hcw94vt9nqevfw8axfnujtn6xsg6eq0q5u2up5"
        val txIdHex = "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2"
        val command = L2LedgerCommand.RegisterDeposit(
          requestId = RequestId(HeadPeerNumber(0), 1L),
          blockNumber = BlockNumber(100),
          blockCreationStartTime = BigInt(1700000000),
          depositId = TransactionInput(
            transactionId =
                Hash[Blake2b_256, HashPurpose.TransactionHash](ByteString.fromHex(txIdHex)),
            index = 0
          ),
          depositFee = Coin(2_000_000L),
          depositL2Value = Value(Coin(10_000_000L)),
          refundDestination = Destination(Address.fromBech32(refundAddress), None),
          l2Payload = ByteString.fromHex("cafebabe")
        )
        val expected = io.circe.parser
            .parse(
              s"""{
                 |  "requestId": 1,
                 |  "blockNumber": 100,
                 |  "blockCreationStartTime": 1700000000,
                 |  "depositId": {"transaction_id": "$txIdHex", "index": 0},
                 |  "depositFee": 2000000,
                 |  "depositL2Value": {"assets": [{"asset": {"tag": "Ada"}, "value": 10000000}]},
                 |  "refundDestination": {"address": "$refundAddress", "datum": null},
                 |  "l2Payload": "cafebabe"
                 |}""".stripMargin
            )
            .toOption
            .get
        assert(command.asJson == expected)
    }

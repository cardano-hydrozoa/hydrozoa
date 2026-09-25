package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** [[RequestCursors]] is the weaver's gate on the per-author request streams, so the properties
  * that matter are the two directions of one rule: a stream that arrives as the peers author it is
  * always admitted, and any other order is always refused.
  */
class RequestCursorsTest extends AnyFlatSpec, Matchers, ScalaCheckPropertyChecks {

    private val genPeerNum: Gen[HeadPeerNumber] = Gen.choose(0, 9).map(HeadPeerNumber(_))

    /** Per-peer streams `0, 1, 2, …`, interleaved arbitrarily across peers — what the request lanes
      * deliver, one contiguous stream per author with no cross-author ordering.
      */
    private val genInterleavedStreams: Gen[List[RequestId]] = for {
        peerNums <- Gen.nonEmptyListOf(genPeerNum).map(_.distinct)
        lengths <- Gen.listOfN(peerNums.size, Gen.choose(1, 12))
        arrivals = peerNums
            .zip(lengths)
            .flatMap((peerNum, length) =>
                (0L until length.toLong).map(n => RequestId(peerNum, RequestNumber(n)))
            )
        shuffled <- Gen.pick(arrivals.size, arrivals)
    } yield
        // `pick` shuffles, which breaks each author's own order; sorting by request number puts
        // every author's stream back in order while leaving the cross-author interleaving alone.
        shuffled.toList.sortBy(id => id.requestNum: Long)

    private def acceptAll(
        cursors: RequestCursors,
        requestIds: List[RequestId]
    ): Either[String, RequestCursors] =
        requestIds.foldLeft[Either[String, RequestCursors]](Right(cursors))((acc, id) =>
            acc.flatMap(_.accept(id))
        )

    it should "admit every peer's stream in order, however the streams interleave" in {
        forAll(genInterleavedStreams) { requestIds =>
            acceptAll(RequestCursors.cold, requestIds) shouldBe a[Right[?, ?]]
        }
    }

    it should "leave each peer's high-water at that peer's last request" in {
        forAll(genInterleavedStreams) { requestIds =>
            val expected = requestIds
                .groupBy(_.peerNum)
                .view
                .mapValues(ids => ids.map(id => id.requestNum: Long).max)
                .toMap
            acceptAll(RequestCursors.cold, requestIds).map(
              _.highWater.view.mapValues(n => n: Long).toMap
            ) shouldBe Right(expected)
        }
    }

    it should "refuse a repeated request number" in {
        forAll(genInterleavedStreams) { requestIds =>
            acceptAll(RequestCursors.cold, requestIds ++ requestIds.headOption.toList) shouldBe a[
              Left[?, ?]
            ]
        }
    }

    it should "refuse a gap" in {
        forAll(genPeerNum, Gen.choose(1L, 40L)) { (peerNum, skipTo) =>
            RequestCursors.cold
                .accept(RequestId(peerNum, RequestNumber(skipTo))) shouldBe a[Left[?, ?]]
        }
    }

    it should "refuse a reordering within one peer's stream" in {
        forAll(genPeerNum, Gen.choose(1L, 40L)) { (peerNum, length) =>
            val stream = (0L to length).map(n => RequestId(peerNum, RequestNumber(n))).toList
            // Swapping the last two arrivals is the smallest possible reordering.
            val reordered = stream.dropRight(2) ++ stream.takeRight(2).reverse
            acceptAll(RequestCursors.cold, reordered) shouldBe a[Left[?, ?]]
        }
    }

    it should "admit the request after a persisted high-water" in {
        forAll(genPeerNum, Gen.choose(0L, 40L)) { (peerNum, highWater) =>
            RequestCursors
                .resume(Map(peerNum -> RequestNumber(highWater)))
                .accept(RequestId(peerNum, RequestNumber(highWater + 1))) shouldBe a[Right[?, ?]]
        }
    }

    it should "refuse the request at a persisted high-water, already included in a block" in {
        forAll(genPeerNum, Gen.choose(0L, 40L)) { (peerNum, highWater) =>
            RequestCursors
                .resume(Map(peerNum -> RequestNumber(highWater)))
                .accept(RequestId(peerNum, RequestNumber(highWater))) shouldBe a[Left[?, ?]]
        }
    }
}

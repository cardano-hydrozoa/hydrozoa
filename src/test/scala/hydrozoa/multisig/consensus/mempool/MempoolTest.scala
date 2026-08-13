package hydrozoa.multisig.consensus.mempool

import hydrozoa.multisig.consensus.UserRequest.TransactionRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}
import scalus.uplc.builtin.ByteString

/** Properties for [[Mempool]]. The dummy request content is irrelevant — only the request id (peer
  * number + arrival slot) drives every behaviour under test: dedup, arrival-order preservation, the
  * leader's own-preference-under-cap extraction, and the follower's stop-at-first-missing replay.
  */
object MempoolTest extends Properties("Mempool") {

    private val poolPeers = 0 to 4
    private val poolNums = 0L to 19L
    // A peer that never appears in a generated mempool, for "prefer an absent peer" (⇒ pure arrival
    // order) and "extract a missing id" cases.
    private val absentPeer = HeadPeerNumber(7)
    private val missingId = RequestId(7, 0)

    private def req(id: RequestId): UserRequestWithId =
        UserRequestWithId(TransactionRequest(TransactionRequestBody(ByteString.empty)), id)

    private val idPool: Vector[RequestId] =
        (for { p <- poolPeers; n <- poolNums } yield RequestId(p, n)).toVector

    /** A list of requests with distinct ids, in a random arrival order. */
    private val genRequests: Gen[List[UserRequestWithId]] =
        for {
            n <- Gen.choose(0, idPool.size)
            ids <- Gen.pick(n, idPool)
        } yield ids.iterator.map(req).toList

    private def build(rs: List[UserRequestWithId]): Mempool =
        rs.foldLeft(Mempool.empty)((m, r) => m.addRequest(r).getOrElse(m))

    /** The surviving requests, in arrival order (extract-all preferring an absent peer). */
    private def inOrder(m: Mempool): List[UserRequestWithId] =
        m.extractInOrderPreferring(absentPeer, Int.MaxValue)._1

    val _ = property("addRequest dedups by id and counts live requests") = forAll(genRequests) {
        rs =>
            val m = build(rs)
            val dupRejected = rs.forall(r => m.addRequest(r).isEmpty)
            (m.size == rs.size) :| s"size ${m.size} != ${rs.size}" &&
            dupRejected :| "a duplicate id was accepted"
    }

    val _ = property("getRequest finds present ids and misses absent ones") = forAll(genRequests) {
        rs =>
            val m = build(rs)
            rs.forall(r => m.getRequest(r.requestId).contains(r)) &&
            m.getRequest(missingId).isEmpty
    }

    val _ = property(
      "extractInOrderPreferring with a big limit and absent peer returns arrival order"
    ) = forAll(genRequests) { rs =>
        val m = build(rs)
        val (chosen, surviving) = m.extractInOrderPreferring(absentPeer, rs.size)
        (chosen == rs) :| s"chosen $chosen != $rs" &&
        surviving.isEmpty :| "surviving not empty"
    }

    val _ = property(
      "extractInOrderPreferring leads own requests then others, each in arrival order, capped at limit"
    ) = forAll(genRequests, Gen.choose(0, 4), Gen.choose(0, 30)) { (rs, peer, limit) =>
        val preferred = HeadPeerNumber(peer)
        val m = build(rs)
        val (chosen, surviving) = m.extractInOrderPreferring(preferred, limit)

        val own = rs.filter(_.requestId.peerNum == preferred)
        val others = rs.filterNot(_.requestId.peerNum == preferred)
        val expected = (own ++ others).take(limit)

        (chosen == expected) :| s"chosen $chosen != expected $expected" &&
        // The surviving mempool keeps the remainder in arrival order (nothing lost or reordered).
        (inOrder(surviving) == rs.filterNot(expected.contains)) :| "surviving order wrong"
    }

    val _ = property(
      "extractRequestsWhile is Complete when every id is present, in the requested order"
    ) = forAll(genRequests) { rs =>
        val m = build(rs)
        forAll(Gen.choose(0, rs.size).flatMap(k => Gen.pick(k, rs))) { picked =>
            val ids = picked.iterator.map(_.requestId).toList
            m.extractRequestsWhile(ids) match {
                case Mempool.Extraction.Complete(extracted, surviving) =>
                    (extracted.map(_.requestId) == ids) :| "extracted order != requested" &&
                    (surviving.size == rs.size - ids.size) :| "surviving size wrong" &&
                    (inOrder(surviving) == rs.filterNot(r => ids.contains(r.requestId))) :|
                        "surviving order wrong"
                case other => false :| s"expected Complete, got $other"
            }
        }
    }

    val _ = property("extractRequestsWhile is Incomplete and stops at the first missing id") =
        forAll(genRequests) { rs =>
            val m = build(rs)
            forAll(Gen.choose(0, rs.size).flatMap(k => Gen.pick(k, rs))) { prefixPick =>
                val prefixIds = prefixPick.iterator.map(_.requestId).toList
                // A present tail after the missing id must be ignored (extraction stops at missing).
                val ids = prefixIds ::: missingId :: rs.map(_.requestId)
                m.extractRequestsWhile(ids) match {
                    case Mempool.Extraction.Incomplete(extracted, surviving, awaiting) =>
                        (awaiting == missingId) :| s"awaiting $awaiting != $missingId" &&
                        (extracted.map(_.requestId) == prefixIds) :| "extracted != prefix" &&
                        (surviving.size == rs.size - prefixIds.size) :| "surviving size wrong"
                    case other => false :| s"expected Incomplete, got $other"
                }
            }
        }
}

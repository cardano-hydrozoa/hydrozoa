package hydrozoa.l2.consensus.network.outbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.ReqVerKey
import hydrozoa.l2.consensus.network.outbox.OutMsgId.toLong
import hydrozoa.l2.consensus.network.transport.{AnyMsg, ReqAux}
import hydrozoa.l2.consensus.network.transport.AnyMsg.ReqVerKeyMsg
import hydrozoa.node.TestPeer.Alice
import hydrozoa.node.db.DBActor
import munit.{FunSuite, ScalaCheckSuite}
import ox.channels.{Actor, ActorRef}
import ox.{sleep, supervised}

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

class DelivererActorSpec extends ScalaCheckSuite:

    test("Deliverer initialization for the clean slate"):
        supervised {
            val db: ActorRef[DBActor] = Actor.create(InMemoryDBActor())
            val outbox = Actor.create(OutboxActor(db))
            val peerId = "tellMeTwice"
            val peer = TellMeNTimesReceiverFixture(2, peerId)
            val deliverer = DelivererActor.initialize(db, outbox, peer)

            assertEquals(peer.counter.get(), 2L)
            assertEquals(deliverer.currentMatchIndex, MatchIndex(0L))
            assertEquals(outbox.ask(_.isSubscribed(peerId)), true)
        }

    test("Handle a message"):
        supervised {
            val db: ActorRef[DBActor] = Actor.create(InMemoryDBActor())
            val outbox = Actor.create(OutboxActor(db))
            val peerId = "tellMeTwice"
            val peer = TellMeNTimesReceiverFixture(2, peerId)
            val deliverer = DelivererActor.initialize(db, outbox, peer)

            assertEquals(peer.counter.get(), 2L)
            assertEquals(deliverer.currentMatchIndex, MatchIndex(0L))
            assertEquals(outbox.ask(_.isSubscribed(peerId)), true)

            // handle anyMsg
            val anyMsg = ReqVerKeyMsg(ReqVerKey(), ReqAux(Alice, 1))
            val outMsgId = outbox.ask(_.addToOutbox(anyMsg))

            sleep(1.second)

            assertEquals(outMsgId, OutMsgId(1L))
            assertEquals(peer.counter.get(), 4L)
            assertEquals(deliverer.currentMatchIndex, MatchIndex(1L))

        }

class InMemoryDBActor extends DBActor:

    private val log = Logger(getClass)

    private val outboxSeq = AtomicLong(0L)
    private val outbox = mutable.Buffer[AnyMsg]()

    override def persistOutgoingMessage(msg: AnyMsg): OutMsgId =
        outbox.append(msg)
        val outMsgId = outboxSeq.incrementAndGet() |> OutMsgId.apply
        log.info(s"persistOutgoingMessage: persisted $msg with outMsgId=$outMsgId")
        outMsgId

    override def getOutgoingMessages(startWithIncluding: OutMsgId): List[(OutMsgId, AnyMsg)] = {
        val msgIdLong = startWithIncluding.toLong
        val ret = outbox
            .clone()
            .toList
            .drop(msgIdLong.toInt - 1)
            .zipWithIndex
            .map { case (msg, i) =>
                (OutMsgId(i.toLong + msgIdLong), msg)
            }
        log.debug(
          s"getOutgoingMessages: found ${ret.size} entries starting with outMsgId=$startWithIncluding"
        )
        ret
    }

class TellMeNTimesReceiverFixture(n: Int = 2, peerId: String) extends Receiver:

    private val log = Logger(getClass)

    val counter = AtomicLong(0L)

    private val entries = mutable.Buffer[(OutMsgId, AnyMsg)]()

    override def id: String = peerId

    override def appendEntries(newEntries: List[(OutMsgId, AnyMsg)]): Option[MatchIndex] =

        if counter.incrementAndGet() % n == 0 then
            log.info(s"appendEntries: appending $newEntries")

            entries.appendAll(newEntries)
            entries.lastOption match {
                case None              => Some(MatchIndex(0L))
                case Some((lastId, _)) => lastId.toLong |> MatchIndex.apply |> Some.apply
            }
        else
            log.info(s"appendEntries: ignoring $newEntries")
            None

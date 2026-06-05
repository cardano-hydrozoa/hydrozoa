package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalacheck.{Prop, Properties}

/** Pc3/Pc4 plumbing tests for the coil-peer hard-ack relay (§8 of `design/coil-network.md`), one
  * hub head peer serving N coil peers. A coil peer's hard-ack travels up its
  * [[CoilPeerToHeadLiaison]] to the hub's [[HeadPeerToCoilLiaison]], which routes it to BOTH the
  * hub's slow-consensus actor and the [[CoilAckSequencer]]; the sequencer stamps it and relays the
  * resulting `HardAckWithId` down EVERY coil link, so each coil peer hears every coil peer's ack
  * (its own echo included — deduped downstream).
  *
  * Everything around the three new actors is stubbed: the liaison `Config` is a hand-built
  * `OwnPeerPublic.Section & NodeOperationMultisigConfig.Section`, and every consensus actor a
  * liaison might touch is a no-op probe — only the slow-consensus slots record what they receive.
  * This exercises the new lane + relay end-to-end without standing up a full node.
  */
object CoilLiaisonTest extends Properties("Coil liaison plumbing") {

    private val headNum = HeadPeerNumber(0)
    private val headPeerId: PeerId = PeerId.Head(headNum)

    private def ownPublic(pid: PeerId, leads: Boolean, label: String): OwnPeerPublic =
        new OwnPeerPublic {
            override def ownPeerId: PeerId = pid
            override def canLeadFast(blockNum: BlockNumber): Boolean = leads
            override def canLeadSlow(stackNum: StackNumber): Boolean = leads
            override def nextOwnLeaderBlock(after: BlockNumber): Option[BlockNumber] =
                Option.when(leads)(after.increment)
            override def nextOwnSlowLeaderStack(after: StackNumber): Option[StackNumber] =
                Option.when(leads)(after.increment)
            override def ownPeerLabel: String = label
            override def ownPeerIndex: Int = 0
        }

    private def stubConfig(
        pub: OwnPeerPublic
    ): OwnPeerPublic.Section & NodeOperationMultisigConfig.Section =
        new OwnPeerPublic.Section with NodeOperationMultisigConfig.Section {
            override def ownPeerPublic: OwnPeerPublic = pub
            override def nodeOperationMultisigConfig: NodeOperationMultisigConfig =
                NodeOperationMultisigConfig.default
        }

    // The head leads every block/stack (sole leader at one head); coil peers never lead.
    private val headConfig = stubConfig(ownPublic(headPeerId, leads = true, "0"))
    private def coilConfig(coilNum: CoilPeerNumber) =
        stubConfig(ownPublic(PeerId.Coil(coilNum), leads = false, s"c${coilNum.convert}"))

    /** A no-op actor for the Connections slots the liaisons never read in this scenario. */
    private def noop[R](system: ActorSystem[IO]): IO[ActorRef[IO, R]] =
        system.actorOf(new Actor[IO, R] {
            override def receive: Receive[IO, R] = PartialFunction.fromFunction((_: R) => IO.unit)
        })

    /** Records every hard-ack handed to a slow-consensus slot. */
    private class HardAckRecorder(seen: Ref[IO, Vector[HardAck]])
        extends Actor[IO, SlowConsensusActor.Request] {
        override def receive: Receive[IO, SlowConsensusActor.Request] = {
            case h: HardAck => seen.update(_ :+ h)
            case _          => IO.unit
        }
    }

    private def coilAck(coilNum: CoilPeerNumber, hardAckNum: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Coil(coilNum), HardAckNumber(hardAckNum)),
          stackNum = StackNumber(hardAckNum),
          payload = HardAck.Round1Payload.Initial(
            fallbackSig = TxSignature(IArray[Byte](7.toByte, 8.toByte, 9.toByte))
          )
        )

    private def mkConnections(
        system: ActorSystem[IO],
        slowConsensus: SlowConsensusActor.Handle,
        headPeerLiaisons: List[PeerLiaison.Handle],
        remotePeerLiaisons: Map[PeerId, PeerLiaison.Handle],
        coilAckSequencer: Option[CoilAckSequencer.Handle],
    ): IO[MultisigRegimeManager.Connections] =
        for {
            blockWeaver <- noop[BlockWeaver.Request](system)
            cardanoLiaison <- noop[CardanoLiaison.Request](system)
            consensusActor <- noop[FastConsensusActor.Request](system)
            eventSequencer <- noop[EventSequencer.Request](system)
            jointLedger <- noop[JointLedger.Requests.Request](system)
            stackComposer <- noop[StackComposer.Request](system)
        } yield MultisigRegimeManager.Connections(
          blockWeaver = blockWeaver,
          blockWeaverLimiter = blockWeaver,
          cardanoLiaison = cardanoLiaison,
          consensusActor = consensusActor,
          eventSequencer = eventSequencer,
          jointLedger = jointLedger,
          stackComposer = stackComposer,
          stackComposerLimiter = stackComposer,
          slowConsensusActor = slowConsensus,
          headPeerLiaisons = headPeerLiaisons,
          remotePeerLiaisons = remotePeerLiaisons,
          coilAckSequencer = coilAckSequencer,
        )

    /** Per-coil actors + the Ref recording what its slow-consensus slot receives. */
    private final case class CoilParts(
        coilNum: CoilPeerNumber,
        pending: Deferred[IO, MultisigRegimeManager.Connections],
        coilSeen: Ref[IO, Vector[HardAck]],
        coilSlowConsensus: SlowConsensusActor.Handle,
        headLiaison: HeadPeerToCoilLiaison.Handle,
        coilLiaison: CoilPeerToHeadLiaison.Handle,
    )

    /** Stand up one hub head serving `acksByCoil.size` coil peers, inject each coil peer's
      * hard-acks, and return what the hub and each coil peer observed once the relay settles.
      */
    private def runRelay(
        acksByCoil: List[List[HardAck]]
    ): (Vector[HardAck], List[Vector[HardAck]]) =
        ActorSystem[IO]("coil-liaison-test")
            .use { system =>
                for {
                    headPending <- Deferred[IO, MultisigRegimeManager.Connections]
                    hubSeen <- Ref[IO].of(Vector.empty[HardAck])
                    hubSlowConsensus <- system.actorOf(new HardAckRecorder(hubSeen))
                    sequencer <- system.actorOf(CoilAckSequencer(headConfig, headPending))

                    coilPeers <- acksByCoil.indices.toList.traverse { i =>
                        val coilNum = CoilPeerNumber(i)
                        for {
                            pending <- Deferred[IO, MultisigRegimeManager.Connections]
                            coilSeen <- Ref[IO].of(Vector.empty[HardAck])
                            coilSlowConsensus <- system.actorOf(new HardAckRecorder(coilSeen))
                            headLiaison <- system.actorOf(
                              HeadPeerToCoilLiaison(
                                headConfig,
                                coilNum,
                                headPending
                              )
                            )
                            coilLiaison <- system.actorOf(
                              CoilPeerToHeadLiaison(
                                coilConfig(coilNum),
                                HeadPeerId(0, 1),
                                pending
                              )
                            )
                        } yield CoilParts(
                          coilNum,
                          pending,
                          coilSeen,
                          coilSlowConsensus,
                          headLiaison,
                          coilLiaison
                        )
                    }

                    headConnections <- mkConnections(
                      system,
                      slowConsensus = hubSlowConsensus,
                      headPeerLiaisons = coilPeers.map(_.headLiaison),
                      remotePeerLiaisons =
                          coilPeers.map(c => PeerId.Coil(c.coilNum) -> c.coilLiaison).toMap,
                      coilAckSequencer = Some(sequencer),
                    )
                    _ <- headPending.complete(headConnections)
                    _ <- coilPeers.traverse_ { c =>
                        mkConnections(
                          system,
                          slowConsensus = c.coilSlowConsensus,
                          headPeerLiaisons = List(c.coilLiaison),
                          remotePeerLiaisons = Map(headPeerId -> c.headLiaison),
                          coilAckSequencer = None,
                        ).flatMap(c.pending.complete)
                    }

                    // Let the initial GetMsgBatch handshakes settle, inject each coil peer's hard-acks
                    // in order, then let the up-relay-down cascade settle.
                    _ <- system.waitForIdle()
                    _ <- coilPeers.traverse_ { c =>
                        acksByCoil(c.coilNum.convert).traverse_(c.coilLiaison ! _)
                    }
                    _ <- system.waitForIdle()

                    hub <- hubSeen.get
                    perCoil <- coilPeers.traverse(_.coilSeen.get)
                } yield (hub, perCoil)
            }
            .unsafeRunSync()

    private def peerIdsOf(acks: Vector[HardAck]): Set[PeerId] = acks.map(_.ackId.peerId).toSet

    val _ = property(
      "a coil peer hard-ack reaches the hub and is relayed back via the HubHardAckLane"
    ) = {
        val coil0 = CoilPeerNumber(0)
        val (hub, perCoil) = runRelay(List(List(coilAck(coil0, 0))))
        Prop(hub.map(_.ackId.peerId) == Vector(PeerId.Coil(coil0))) :| s"hub saw: $hub" &&
        Prop(perCoil.head.map(_.ackId.peerId) == Vector(PeerId.Coil(coil0))) :|
            s"coil saw: ${perCoil.head}"
    }

    val _ = property("two coil peers each hear both acks via the hub's HubHardAckLane") = {
        val coil0 = CoilPeerNumber(0)
        val coil1 = CoilPeerNumber(1)
        val (hub, perCoil) = runRelay(List(List(coilAck(coil0, 0)), List(coilAck(coil1, 0))))
        val both = Set[PeerId](PeerId.Coil(coil0), PeerId.Coil(coil1))
        Prop(peerIdsOf(hub) == both) :| s"hub saw: $hub" &&
        Prop(perCoil.forall(peerIdsOf(_) == both)) :| s"coil peers saw: $perCoil"
    }

    val _ = property("a coil peer's successive hard-acks are relayed back in order") = {
        val coil0 = CoilPeerNumber(0)
        val acks = List(coilAck(coil0, 0), coilAck(coil0, 1), coilAck(coil0, 2))
        val (hub, perCoil) = runRelay(List(acks))
        val expected = Vector(0, 1, 2)
        Prop(hub.map(_.hardAckNum.convert) == expected) :| s"hub saw: $hub" &&
        Prop(perCoil.head.map(_.hardAckNum.convert) == expected) :| s"coil saw: ${perCoil.head}"
    }
}

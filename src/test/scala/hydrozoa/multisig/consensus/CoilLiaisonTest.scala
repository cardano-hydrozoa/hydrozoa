package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.coil.CoilPeer
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.liaison.{PeerLiaisonCoilToHub, PeerLiaisonHubToCoil}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalacheck.{Prop, Properties}
import test.{SeedPhrase, TestPeers, genMonad}

/** Pc3/Pc4 plumbing tests for the coil-peer hard-ack relay (§8 of `design/coil-network.md`), one
  * hub head peer serving N coil peers. A coil peer's hard-ack travels up its
  * [[PeerLiaisonCoilToHub]] to the hub's [[PeerLiaisonHubToCoil]], which routes it to BOTH the
  * hub's slow-consensus actor and the [[CoilAckSequencer]]; the sequencer stamps it and hands the
  * resulting `HardAckWithId` to the [[CoilRelay]], which fans it down EVERY hub→coil liaison, so
  * each coil peer hears every coil peer's ack (its own echo included — deduped downstream).
  *
  * The configs are real (the head config lists the coil peers so the threshold script and lane
  * topology resolve), but every consensus actor a liaison might touch is a no-op probe — only the
  * slow-consensus slots record what they receive. This exercises the new lane + relay end-to-end
  * without standing up a full node.
  */
object CoilLiaisonTest extends Properties("Coil liaison plumbing") {

    private val network = CardanoNetwork.Preprod
    private val nHeadPeers = 2
    private val hubNum = HeadPeerNumber(0)

    /** A head config (hubbed at head peer 0) plus the hub's node config and one node config per
      * coil peer. Coil wallets are extra keys from the same seed, beyond the head set.
      */
    private def genConfigs(nCoil: Int): (NodeConfig, List[NodeConfig]) = {
        val headPeers = TestPeers.apply(SeedPhrase.Yaci, network, nHeadPeers)
        val allPeers = TestPeers.apply(SeedPhrase.Yaci, network, nHeadPeers + nCoil)
        val coilWallets =
            (0 until nCoil).toList.map(i => allPeers.walletFor(HeadPeerNumber(nHeadPeers + i)))

        val mnc: MultiNodeConfig = MultiNodeConfig
            .generateWith(headPeers)(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = generateHeadConfigBootstrap(
                  generateHeadParams = generateHeadParameters().map(_.copy(coilQuorum = 1)),
                  coilPeers = coilWallets.map(w => CoilPeer(w.exportVerificationKey, hubNum))
                )
              )
            )
            .sample
            .get

        val hubConfig = mnc.nodeConfigs(hubNum)
        val hubPrivate = mnc.nodePrivateConfigs(hubNum)
        val coilConfigs = coilWallets.map(w =>
            NodeConfig
                .mkCoilConfig(
                  headConfig = mnc.headConfig,
                  ownCoilWallet = w,
                  nodeOperationEvacuationConfig = hubPrivate.nodeOperationEvacuationConfig,
                  nodeOperationMultisigConfig = hubPrivate.nodeOperationMultisigConfig,
                  hydrozoaHost = "localhost",
                  hydrozoaPort = "4973",
                  blockfrostApiKey = "not-a-real-key"
                )
                .get
        )
        (hubConfig, coilConfigs)
    }

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

    /** The shared regime Connections with every core actor stubbed as a no-op and the given
      * slow-consensus recorder wired in. Callers `.copy` the coil-specific slots on top.
      */
    private def baseConnections(
        system: ActorSystem[IO],
        slowConsensus: SlowConsensusActor.Handle,
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
        )

    /** Per-coil actors + the Ref recording what its slow-consensus slot receives. */
    private final case class CoilParts(
        coilNum: CoilPeerNumber,
        pending: Deferred[IO, MultisigRegimeManager.Connections],
        coilSeen: Ref[IO, Vector[HardAck]],
        coilSlowConsensus: SlowConsensusActor.Handle,
        coilLiaison: PeerLiaisonCoilToHub.Handle,
        hubLiaison: PeerLiaisonHubToCoil.Handle,
    )

    /** Stand up one hub head serving `nCoil` coil peers, inject each coil peer's hard-acks (built
      * by `mkAcks` from the coil peers' resolved numbers), and return what the hub and each coil
      * peer observed once the relay settles.
      */
    private def runRelay(nCoil: Int)(
        mkAcks: List[CoilPeerNumber] => List[List[HardAck]]
    ): (Vector[HardAck], List[Vector[HardAck]]) = {
        val (hubConfig, coilConfigs) = genConfigs(nCoil)
        val hubHead = HeadPeerId(hubNum.convert, hubConfig.nHeadPeers)

        ActorSystem[IO]("coil-liaison-test")
            .use { system =>
                for {
                    headPending <- Deferred[IO, MultisigRegimeManager.Connections]
                    hubSeen <- Ref[IO].of(Vector.empty[HardAck])
                    hubSlowConsensus <- system.actorOf(new HardAckRecorder(hubSeen))
                    sequencer <- system.actorOf(CoilAckSequencer(hubConfig, headPending))
                    coilRelay <- system.actorOf(CoilRelay(headPending))

                    coilPeers <- coilConfigs.traverse { coilConfig =>
                        val coilNum = coilConfig.ownPeerId match {
                            case PeerId.Coil(n) => n
                            case PeerId.Head(_) =>
                                throw new IllegalStateException("coil config is not a coil peer")
                        }
                        for {
                            pending <- Deferred[IO, MultisigRegimeManager.Connections]
                            coilSeen <- Ref[IO].of(Vector.empty[HardAck])
                            coilSlowConsensus <- system.actorOf(new HardAckRecorder(coilSeen))
                            coilLiaison <- system.actorOf(
                              PeerLiaisonCoilToHub(coilConfig, hubHead, pending)
                            )
                            hubLiaison <- system.actorOf(
                              PeerLiaisonHubToCoil(hubConfig, coilNum, headPending)
                            )
                        } yield CoilParts(
                          coilNum,
                          pending,
                          coilSeen,
                          coilSlowConsensus,
                          coilLiaison,
                          hubLiaison
                        )
                    }

                    headConnections <- baseConnections(system, slowConsensus = hubSlowConsensus)
                        .map(
                          _.copy(
                            coilRelay = Some(coilRelay),
                            coilAckSequencer = Some(sequencer),
                            coilPeerLiaisons = coilPeers.map(_.hubLiaison),
                            remoteCoilLiaisons =
                                coilPeers.map(c => c.coilNum -> c.coilLiaison).toMap,
                          )
                        )
                    _ <- headPending.complete(headConnections)
                    _ <- coilPeers.traverse_ { c =>
                        baseConnections(system, slowConsensus = c.coilSlowConsensus)
                            .map(
                              _.copy(
                                coilUplink = Some(c.coilLiaison),
                                remoteHubLiaison = Some(c.hubLiaison),
                              )
                            )
                            .flatMap(c.pending.complete)
                    }

                    // Let the initial population/own-hard-ack handshakes settle, inject each coil
                    // peer's hard-acks in order, then let the up-relay-down cascade settle.
                    _ <- system.waitForIdle()
                    acksByCoil = mkAcks(coilPeers.map(_.coilNum))
                    _ <- coilPeers.zip(acksByCoil).traverse_ { case (c, acks) =>
                        acks.traverse_(c.coilLiaison ! _)
                    }
                    _ <- system.waitForIdle()

                    hub <- hubSeen.get
                    perCoil <- coilPeers.traverse(_.coilSeen.get)
                } yield (hub, perCoil)
            }
            .unsafeRunSync()
    }

    private def peerIdsOf(acks: Vector[HardAck]): Set[PeerId] = acks.map(_.ackId.peerId).toSet

    val _ = property(
      "a coil peer hard-ack reaches the hub and is relayed back via the HubHardAckLane"
    ) = {
        val (hub, perCoil) = runRelay(1) { case List(c0) =>
            List(List(coilAck(c0, 0)))
        }
        val c0 = perCoil.head.headOption.map(_.ackId.peerId)
        Prop(hub.map(_.ackId.peerId).toSet == perCoil.head.map(_.ackId.peerId).toSet) :|
            s"hub saw: $hub" &&
            Prop(hub.size == 1 && perCoil.head.size == 1) :|
            s"sizes: hub=${hub.size} coil=${perCoil.head.size}" &&
            Prop(c0.exists(_.isInstanceOf[PeerId.Coil])) :| s"coil saw: ${perCoil.head}"
    }

    val _ = property("two coil peers each hear both acks via the hub's HubHardAckLane") = {
        val (hub, perCoil) = runRelay(2) { case List(c0, c1) =>
            List(List(coilAck(c0, 0)), List(coilAck(c1, 0)))
        }
        val both = peerIdsOf(hub)
        Prop(both.size == 2) :| s"hub saw: $hub" &&
        Prop(perCoil.forall(peerIdsOf(_) == both)) :| s"coil peers saw: $perCoil"
    }

    val _ = property("a coil peer's successive hard-acks are relayed back in order") = {
        val (hub, perCoil) = runRelay(1) { case List(c0) =>
            List(List(coilAck(c0, 0), coilAck(c0, 1), coilAck(c0, 2)))
        }
        val expected = Vector(0, 1, 2)
        Prop(hub.map(_.hardAckNum.convert) == expected) :| s"hub saw: $hub" &&
        Prop(perCoil.head.map(_.hardAckNum.convert) == expected) :| s"coil saw: ${perCoil.head}"
    }
}

package hydrozoa.multisig.consensus.transport

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Mesh
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.funsuite.AnyFunSuite

class FirewalledPeerTransportTest extends AnyFunSuite:

    private val ownId: HeadPeerId = HeadPeerId(HeadPeerNumber(0), PositiveInt.unsafeApply(2))
    private val remoteId: HeadPeerId = HeadPeerId(HeadPeerNumber(1), PositiveInt.unsafeApply(2))

    /** An arbitrary wire-eligible message; this suite tests the firewall's forward/drop decision,
      * not what travels.
      */
    private val probe: LiaisonProtocol.MeshEmitted = Mesh.Get(
      batchNum = BatchNumber.zero,
      block = BlockNumber(1),
      stack = StackNumber(1),
      request = RequestNumber.zero,
      requestCeiling = RequestNumber.zero,
      softAck = SoftAckNumber.zero,
      headHardAck = HardAckNumber.zero,
      hubHardAck = HubHardAckNumber.zero
    )

    private def newFakeTransport(
        sends: Ref[IO, List[(HeadPeerId, LiaisonProtocol.MeshEmitted)]]
    ): PeerTransport =
        new PeerTransport:
            override def ownPeerId: HeadPeerId = ownId
            override def register(
                remote: HeadPeerId,
                localLiaison: LiaisonProtocol.MeshLiaisonHandle,
            ): IO[Unit] = IO.unit
            override def send(
                remote: HeadPeerId,
                request: LiaisonProtocol.MeshEmitted,
            ): IO[Unit] =
                sends.update((remote, request) :: _)

    test("send is forwarded when shouldDrop = false; underlying receives it, no drop event") {
        val io = for
            sends <- Ref[IO].of(List.empty[(HeadPeerId, LiaisonProtocol.MeshEmitted)])
            captured <- Ref[IO].of(List.empty[FirewalledPeerTransportEvent])
            underlying = newFakeTransport(sends)
            sink = ContraTracer[IO, FirewalledPeerTransportEvent](e => captured.update(e :: _))
            firewalled = new FirewalledPeerTransport(underlying, _ => IO.pure(false), sink)
            _ <- firewalled.send(remoteId, probe)
            forwarded <- sends.get
            droppedEvents <- captured.get
        yield
            val _ = assert(forwarded == List((remoteId, probe)))
            assert(droppedEvents.isEmpty)
        io.unsafeRunSync()
    }

    test("send is short-circuited when shouldDrop = true; underlying is never asked") {
        val io = for
            sends <- Ref[IO].of(List.empty[(HeadPeerId, LiaisonProtocol.MeshEmitted)])
            captured <- Ref[IO].of(List.empty[FirewalledPeerTransportEvent])
            underlying = newFakeTransport(sends)
            sink = ContraTracer[IO, FirewalledPeerTransportEvent](e => captured.update(e :: _))
            firewalled = new FirewalledPeerTransport(underlying, _ => IO.pure(true), sink)
            _ <- firewalled.send(remoteId, probe)
            forwarded <- sends.get
            droppedEvents <- captured.get
        yield
            val _ = assert(forwarded.isEmpty)
            assert(
              droppedEvents == List(FirewalledPeerTransportEvent.DroppedOutbound(remoteId))
            )
        io.unsafeRunSync()
    }

    test("shouldDrop can key on remote — drop peer 1, forward to peer 2") {
        val other = HeadPeerId(HeadPeerNumber(2), PositiveInt.unsafeApply(3))
        val io = for
            sends <- Ref[IO].of(List.empty[(HeadPeerId, LiaisonProtocol.MeshEmitted)])
            captured <- Ref[IO].of(List.empty[FirewalledPeerTransportEvent])
            underlying = newFakeTransport(sends)
            sink = ContraTracer[IO, FirewalledPeerTransportEvent](e => captured.update(e :: _))
            firewalled = new FirewalledPeerTransport(
              underlying,
              r => IO.pure(r == remoteId),
              sink,
            )
            _ <- firewalled.send(remoteId, probe)
            _ <- firewalled.send(other, probe)
            forwarded <- sends.get
            droppedEvents <- captured.get
        yield
            val _ = assert(forwarded == List((other, probe)))
            assert(
              droppedEvents == List(FirewalledPeerTransportEvent.DroppedOutbound(remoteId))
            )
        io.unsafeRunSync()
    }

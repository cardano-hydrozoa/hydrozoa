package hydrozoa.multisig.consensus.transport

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHeadToHead}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import org.scalatest.funsuite.AnyFunSuite

class FirewalledPeerTransportTest extends AnyFunSuite:

    private val ownId: HeadPeerId = HeadPeerId(HeadPeerNumber(0), PositiveInt.unsafeApply(2))
    private val remoteId: HeadPeerId = HeadPeerId(HeadPeerNumber(1), PositiveInt.unsafeApply(2))

    private def newFakeTransport(
        sends: Ref[IO, List[(HeadPeerId, LiaisonProtocol.HeadToHeadRequest)]]
    ): PeerTransport =
        new PeerTransport:
            override def ownPeerId: HeadPeerId = ownId
            override def register(
                remote: HeadPeerId,
                localLiaison: PeerLiaisonHeadToHead.Handle,
            ): IO[Unit] = IO.unit
            override def send(
                remote: HeadPeerId,
                request: LiaisonProtocol.HeadToHeadRequest,
            ): IO[Unit] =
                sends.update((remote, request) :: _)

    test("send is forwarded when shouldDrop = false; underlying receives it, no drop event") {
        val io = for
            sends <- Ref[IO].of(List.empty[(HeadPeerId, LiaisonProtocol.HeadToHeadRequest)])
            captured <- Ref[IO].of(List.empty[FirewalledPeerTransportEvent])
            underlying = newFakeTransport(sends)
            sink = ContraTracer[IO, FirewalledPeerTransportEvent](e => captured.update(e :: _))
            firewalled = new FirewalledPeerTransport(underlying, _ => IO.pure(false), sink)
            _ <- firewalled.send(remoteId, LiaisonProtocol.ResendCurrent)
            forwarded <- sends.get
            droppedEvents <- captured.get
        yield
            val _ = assert(forwarded == List((remoteId, LiaisonProtocol.ResendCurrent)))
            assert(droppedEvents.isEmpty)
        io.unsafeRunSync()
    }

    test("send is short-circuited when shouldDrop = true; underlying is never asked") {
        val io = for
            sends <- Ref[IO].of(List.empty[(HeadPeerId, LiaisonProtocol.HeadToHeadRequest)])
            captured <- Ref[IO].of(List.empty[FirewalledPeerTransportEvent])
            underlying = newFakeTransport(sends)
            sink = ContraTracer[IO, FirewalledPeerTransportEvent](e => captured.update(e :: _))
            firewalled = new FirewalledPeerTransport(underlying, _ => IO.pure(true), sink)
            _ <- firewalled.send(remoteId, LiaisonProtocol.ResendCurrent)
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
            sends <- Ref[IO].of(List.empty[(HeadPeerId, LiaisonProtocol.HeadToHeadRequest)])
            captured <- Ref[IO].of(List.empty[FirewalledPeerTransportEvent])
            underlying = newFakeTransport(sends)
            sink = ContraTracer[IO, FirewalledPeerTransportEvent](e => captured.update(e :: _))
            firewalled = new FirewalledPeerTransport(
              underlying,
              r => IO.pure(r == remoteId),
              sink,
            )
            _ <- firewalled.send(remoteId, LiaisonProtocol.ResendCurrent)
            _ <- firewalled.send(other, LiaisonProtocol.ResendCurrent)
            forwarded <- sends.get
            droppedEvents <- captured.get
        yield
            val _ = assert(forwarded == List((other, LiaisonProtocol.ResendCurrent)))
            assert(
              droppedEvents == List(FirewalledPeerTransportEvent.DroppedOutbound(remoteId))
            )
        io.unsafeRunSync()
    }

package hydrozoa.app

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.http4s.Uri
import org.http4s.implicits.uri
import org.scalatest.funsuite.AnyFunSuite

/** [[Serve.peerBindAddress]]: where the peer websocket server listens, versus where the shared head
  * config says to dial this peer.
  *
  * They coincide on a flat network, and stop coinciding the moment something terminates connections
  * in front of the node -- a mutual-TLS proxy, say. Then peers dial a public name that resolves to
  * the proxy, which no host can bind, and the node has to be told where to listen instead.
  */
class PeerBindAddressTest extends AnyFunSuite:

    private val peer0 = HeadPeerNumber(0)

    test("with no override, the node binds exactly what the head config advertises") {
        val (host, port) = Serve.peerBindAddress(None, None, uri"ws://127.0.0.1:4001", peer0)
        assert(host.toString == "127.0.0.1")
        assert(port.value == 4001)
    }

    test("behind a proxy, the advertised name is unbindable and the override decides") {
        // The head advertises the public name so coils know where to dial; the node
        // listens on every interface so the proxy in front can reach it.
        val (host, port) = Serve.peerBindAddress(
          Some("0.0.0.0"),
          Some("4001"),
          uri"wss://head.sugar.rush.sundae.fi:4001",
          peer0
        )
        assert(host.toString == "0.0.0.0")
        assert(port.value == 4001)
    }

    test("host and port override independently") {
        val advertised = uri"wss://head.sugar.rush.sundae.fi:4001"
        val (h1, p1) = Serve.peerBindAddress(Some("0.0.0.0"), None, advertised, peer0)
        assert(
          h1.toString == "0.0.0.0" && p1.value == 4001,
          "port falls back to the advertised one"
        )

        val (h2, p2) = Serve.peerBindAddress(None, Some("14001"), advertised, peer0)
        assert(h2.toString == "head.sugar.rush.sundae.fi", "host falls back to the advertised one")
        assert(p2.value == 14001)
    }

    test("a malformed override fails loudly rather than silently binding somewhere else") {
        val advertised = uri"ws://127.0.0.1:4001"
        assertThrows[IllegalArgumentException] {
            Serve.peerBindAddress(None, Some("not-a-port"), advertised, peer0)
        }
        assertThrows[IllegalArgumentException] {
            Serve.peerBindAddress(None, Some("70000"), advertised, peer0)
        }
    }

    test("an advertised address with no port is still an error when nothing overrides it") {
        assertThrows[IllegalArgumentException] {
            Serve.peerBindAddress(None, None, uri"ws://127.0.0.1", peer0)
        }
    }

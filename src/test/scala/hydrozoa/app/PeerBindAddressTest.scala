package hydrozoa.app

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
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
    private val public = uri"wss://head.sugar.rush.sundae.fi:4001"
    private val local = uri"ws://127.0.0.1:4001"

    test("with no override, the node binds exactly what the head config advertises") {
        val (host, port) = Serve.peerBindAddress(None, None, local, peer0)
        assert(host.toString == "127.0.0.1" && port.value == 4001)
    }

    test("behind a proxy, the advertised name is unbindable and the override decides") {
        // The head advertises the public name so coils know where to dial; the node
        // listens on every interface so the proxy in front can reach it.
        val (host, port) = Serve.peerBindAddress(Some("0.0.0.0"), Some("4001"), public, peer0)
        assert(host.toString == "0.0.0.0" && port.value == 4001)
    }

    test("overriding only the host leaves the advertised port in place") {
        val (host, port) = Serve.peerBindAddress(Some("0.0.0.0"), None, public, peer0)
        assert(host.toString == "0.0.0.0" && port.value == 4001)
    }

    test("overriding only the port leaves the advertised host in place") {
        val (host, port) = Serve.peerBindAddress(None, Some("14001"), public, peer0)
        assert(host.toString == "head.sugar.rush.sundae.fi" && port.value == 14001)
    }

    test("a non-numeric port override fails loudly rather than binding somewhere else") {
        assertThrows[IllegalArgumentException] {
            Serve.peerBindAddress(None, Some("not-a-port"), local, peer0)
        }
    }

    test("an out-of-range port override fails loudly too") {
        assertThrows[IllegalArgumentException] {
            Serve.peerBindAddress(None, Some("70000"), local, peer0)
        }
    }

    test("an advertised address with no port is still an error when nothing overrides it") {
        assertThrows[IllegalArgumentException] {
            Serve.peerBindAddress(None, None, uri"ws://127.0.0.1", peer0)
        }
    }

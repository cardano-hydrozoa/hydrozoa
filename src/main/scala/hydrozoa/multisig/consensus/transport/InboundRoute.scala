package hydrozoa.multisig.consensus.transport

/** Where a transport sends the inbound frames of one link.
  *
  * A link with no route at all has never had a liaison registered; one with a route has had one,
  * and either still routes to it or was closed on purpose. The two absent-liaison cases read
  * differently: a frame on a never-registered link is a wiring fault or an early frame, while a
  * frame on a closed one is the other end not yet knowing that this node stopped its liaison (it
  * handed off to the rule-based regime), and is expected.
  */
enum InboundRoute[+H]:

    /** Inbound goes to this local liaison. */
    case Live(liaison: H)

    /** The liaison was unregistered on purpose; inbound is dropped. */
    case Closed

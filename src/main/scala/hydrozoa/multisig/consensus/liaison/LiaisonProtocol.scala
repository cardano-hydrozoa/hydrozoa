package hydrozoa.multisig.consensus.liaison

import cats.effect.IO
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckWithId, SoftAck}
import hydrozoa.multisig.consensus.{HardConfirmedHighWater, SoftConfirmedHighWater, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.stack.StackBrief

/** The message protocol shared by the three liaison actors (§5.5 of `docs/spec/coil-network.md`)
  * [doc-ref].
  *
  * Each actor's `Request` and `Handle` live here rather than in its own companion to cut a compile
  * cycle between the two hub↔coil liaisons. The cycle is mutual: a [[PeerLiaisonHubToCoil]] sends
  * to its coil peer, so it holds a `CoilToHubHandle`; a [[PeerLiaisonCoilToHub]] sends to its hub,
  * so it holds a `HubToCoilHandle`. If those `Handle` aliases lived in the actors' own companions,
  * typing `PeerLiaisonHubToCoil` would force resolving `PeerLiaisonCoilToHub.Handle` (a companion
  * MEMBER, not just the class type) — which forces typing `PeerLiaisonCoilToHub`, which needs
  * `PeerLiaisonHubToCoil.Handle` back: a genuine cycle that mutual companion-member access does not
  * survive.
  *
  * Hoisting the `Request` unions and `Handle` aliases here breaks it. Both actors now depend on
  * this object, and this object depends on NEITHER — a `Handle` is just `ActorRef[IO, <a union of
  * plain payload types>]`, with no reference back to the actor classes. So `LiaisonProtocol` types
  * first, then each liaison types independently against these aliases; the actor bodies still talk
  * to each other, but only through types owned by a third party neither needs compiled first. Each
  * companion re-exposes its own alias (e.g. `PeerLiaisonHubToCoil.Handle`) for ergonomics, but the
  * canonical definition is here, so resolution never bounces between the two companions.
  *
  * A liaison receives, besides the control ticks: the **batch messages** of its two link halves
  * (one pull, one serve), and the **artifacts** local actors hand it to append to an outbox lane.
  * The appended artifact carries its own author, so the actor routes it to the right per-author
  * lane by inspecting the payload — no separate author argument.
  */
object LiaisonProtocol {

    /** Start tick — sent to self at `preStart` to wire connections and open the pull chain. */
    case object PreStart

    /** Retransmit tick — periodic self-message that re-sends the outstanding pull (self-heals the
      * chain after a wire-level loss).
      */
    case object ResendCurrent

    /** Join-wait tick — self-message telling a coil liaison in join mode to stop waiting for its
      * hub's answer and become the regular liaison. Armed only for a warm coil; a cold one has
      * nothing to walk forward from and waits indefinitely (`CoilJoin.joinWait`).
      */
    case object JoinWaitElapsed

    /** The ticks EVERY liaison takes. Deliberately not [[JoinWaitElapsed]]: only a coil liaison has
      * a join mode to leave, so putting that tick here would hand `PeerLiaisonHeadToHead` and
      * `PeerLiaisonHubToCoil` a message neither can ever receive — the same conflation this
      * object's handle types exist to avoid.
      */
    type Control = PreStart.type | ResendCurrent.type

    // ---- Send vocabularies ----------------------------------------------------------------------
    // What each party may put on a link, and what local actors may hand a liaison. These are the
    // pieces the inboxes below are assembled from, and the element types of the handles at the
    // foot of this object. A send vocabulary is never an inbox: the two sets differ at every one
    // of these links, and conflating them is what this object exists to stop.

    /** What either end of a head↔head link may put on it. Symmetric, unlike the hub↔coil pair: the
      * mesh is peer-to-peer, so both ends pull with [[BatchMessages.Mesh.Get]] and serve with
      * [[BatchMessages.Mesh.New]].
      */
    type MeshEmitted = BatchMessages.Mesh.Get | BatchMessages.Mesh.New

    /** What a hub may put on a hub↔coil link: its answer to a handshake, the population it serves,
      * and its pull of the coil peer's own hard-ack.
      *
      * It is deliberately not [[CoilRequestServed]]: a coil liaison also takes control ticks and
      * local artifacts no hub can send. `ActorRef` is contravariant in its request, so a handle
      * typed here accepts the proxy that forwards to the transport with no widening at the call
      * site.
      */
    type HubEmitted =
        BatchMessages.Join.Offer | BatchMessages.Join.NoOffer | BatchMessages.Population.New |
            BatchMessages.OwnHardAck.Get

    /** What a coil may put on a hub↔coil link: its pull of the population, and its own hard-ack
      * served to the hub. Read the same way as [[HubEmitted]].
      */
    type CoilEmitted = BatchMessages.Population.Get | BatchMessages.OwnHardAck.New

    /** What a coil node's own actors hand their local liaison: the hard-ack to append, and the two
      * confirmation notifications its pull ceilings are anchored on
      * (design/liaison-backpressure.md). Nothing here crosses the wire, and nothing a hub sends
      * belongs in it.
      */
    type CoilUplink = HardAck | SoftConfirmedHighWater | HardConfirmedHighWater

    /** What a head peer's own actors hand a liaison to append to an outbox lane.
      *
      * The same set on both of a head peer's liaison kinds: a hub relays its own production to its
      * coil peers exactly as it relays it across the mesh.
      */
    type Artifacts =
        BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck | HardAckWithId

    // ---- Inboxes --------------------------------------------------------------------------------
    // Each reads the same way: the ticks this liaison sends itself, what its peer may put on the
    // link, and what its own local actors hand it. Anything that appears on one line and not the
    // others is genuinely one-sided, and visibly so.

    /** Head ↔ head: serves and pulls one head peer's own production, and accepts that head peer's
      * artifacts to append to its outbox. [[SoftConfirmedHighWater]] is the local notification its
      * request ceiling is anchored on.
      */
    type HeadToHeadRequest = Control | MeshEmitted | SoftConfirmedHighWater | Artifacts

    /** Hub → coil: serves this coil peer's population pull, pulls its own hard-ack, and accepts the
      * population artifacts `CoilRelay` hands it.
      *
      * [[BatchMessages.Join.Connected]] arrives from the transport, not the wire: the hub side of
      * the link turns an accepted handshake into it so the start-point decision is made where the
      * store is, not in the transport. Hub-only, and the reason this line is not simply
      * `Control | CoilEmitted | Artifacts`.
      */
    type HubRequestServed = Control | BatchMessages.Join.Connected | CoilEmitted | Artifacts

    /** Coil → hub: takes everything its hub may send, plus what its own actors hand it.
      *
      * [[JoinWaitElapsed]] is coil-only — no other liaison has a join mode to leave — which is why
      * it sits here rather than in [[Control]].
      */
    type CoilRequestServed = Control | JoinWaitElapsed.type | HubEmitted | CoilUplink

    // ---- Handles --------------------------------------------------------------------------------

    /** ⚠️ The one handle still typed at the receiving actor's **inbox** rather than at what a head
      * peer may send: it carries `Control` and [[Artifacts]] that never cross a mesh link.
      * [[MeshEmitted]] is the vocabulary it wants, and retyping it is the head-mesh half of
      * GUM-352, left out of that work item.
      */
    type HeadToHeadHandle = ActorRef[IO, HeadToHeadRequest]

    /** The **coil's** handle to its hub: it carries what a coil may emit, not the hub's inbox. */
    type HubToCoilHandle = ActorRef[IO, CoilEmitted]

    /** The **hub's** handle to one coil peer: it carries what a hub may emit, not the coil's inbox.
      */
    type CoilToHubHandle = ActorRef[IO, HubEmitted]

    /** A coil node's handle to its own liaison, for local appends and notifications. */
    type CoilUplinkHandle = ActorRef[IO, CoilUplink]
}

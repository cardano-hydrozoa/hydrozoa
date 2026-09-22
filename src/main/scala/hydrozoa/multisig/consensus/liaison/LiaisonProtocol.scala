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
  * Each actor's message union and `Handle` live here rather than in its own companion to cut a
  * compile cycle between the two hub↔coil liaisons. The cycle is mutual: a [[PeerLiaisonHubToCoil]]
  * sends to its coil peer, so it holds a `CoilToHubHandle`; a [[PeerLiaisonCoilToHub]] sends to its
  * hub, so it holds a `HubToCoilHandle`. If those `Handle` aliases lived in the actors' own
  * companions, typing `PeerLiaisonHubToCoil` would force resolving `PeerLiaisonCoilToHub.Handle` (a
  * companion MEMBER, not just the class type) — which forces typing `PeerLiaisonCoilToHub`, which
  * needs `PeerLiaisonHubToCoil.Handle` back: a genuine cycle that mutual companion-member access
  * does not survive.
  *
  * Hoisting the message unions and `Handle` aliases here breaks it. Both actors now depend on this
  * object, and this object depends on NEITHER — a `Handle` is just `ActorRef[IO, <a union of plain
  * payload types>]`, with no reference back to the actor classes. So `LiaisonProtocol` types first,
  * then each liaison types independently against these aliases; the actor bodies still talk to each
  * other, but only through types owned by a third party neither needs compiled first. Each
  * companion re-exposes its own alias (e.g. `PeerLiaisonHubToCoil.Handle`) for ergonomics, but the
  * canonical definition is here, so resolution never bounces between the two companions.
  *
  * A liaison receives, besides the control ticks: the **batch messages** of its two link halves
  * (one pull, one serve), the **production** local actors hand it to append to an outbox lane, and
  * the **confirmations** its pull ceilings are anchored on. The appended item carries its own
  * author, so the actor routes it to the right per-author lane by inspecting the payload — no
  * separate author argument.
  */
object LiaisonProtocol {

    /** Start tick — sent to self at `preStart` to wire connections and open the pull chain. */
    case object PreStart

    /** Retransmit tick — periodic self-message that re-sends the outstanding pull (self-heals the
      * chain after a wire-level loss).
      */
    case object ResendCurrent

    /** Join-wait tick — self-message telling a coil liaison in join mode to stop waiting for its
      * hub's answer and become the regular liaison. Armed from `CoilJoin.JoinWait`: only for a warm
      * coil, since a cold one has nothing to walk forward from and waits indefinitely.
      *
      * It reports that time passed, carrying nothing from the other end of the link.
      */
    case object JoinWaitElapsed

    /** The ticks every liaison takes. [[JoinWaitElapsed]] is not among them: only a coil liaison
      * has a join mode to leave.
      */
    type Control = PreStart.type | ResendCurrent.type

    // ---- Link vocabularies ----------------------------------------------------------------------
    // What each party may put on a link. These are the element types of the handles at the foot of
    // this object, and half of what each liaison's message union is assembled from.

    /** What either end of a head↔head link may put on it. Symmetric, unlike the hub↔coil pair: the
      * mesh is peer-to-peer, so both ends pull with [[BatchMessages.Mesh.Get]] and serve with
      * [[BatchMessages.Mesh.New]].
      */
    type MeshEmitted = BatchMessages.Mesh.Get | BatchMessages.Mesh.New

    /** What a hub may put on a hub↔coil link: its answer to a handshake, the population it serves,
      * and its pull of the coil peer's own hard-ack.
      *
      * `ActorRef` is contravariant in its request, so a handle typed here accepts the proxy that
      * forwards to the transport with no widening at the call site.
      */
    type HubEmitted =
        BatchMessages.Join.Offer | BatchMessages.Join.NoOffer | BatchMessages.Population.New |
            BatchMessages.OwnHardAck.Get

    /** What a coil may put on a hub↔coil link: its pull of the population, and its own hard-ack
      * served to the hub. Read the same way as [[HubEmitted]].
      */
    type CoilEmitted = BatchMessages.Population.Get | BatchMessages.OwnHardAck.New

    // ---- Local vocabularies ---------------------------------------------------------------------
    // What a node's OWN actors and transport hand a liaison. None of it crosses a link, and each
    // set is the union of two roles: the production to append to an outbox lane, and the
    // confirmation notifications a pull ceiling is anchored on.
    //
    // **Ceiling anchors follow the puller.** A liaison needs those notifications exactly when it
    // computes a ceiling to put in its own pull. [[BatchMessages.Population.Get]] carries four of
    // them, so a hub reads the bound out of the request it serves.

    /** What a head peer's own actors hand a liaison to append to an outbox lane.
      *
      * The same set on both of a head peer's liaison kinds: a hub relays its own production to its
      * coil peers exactly as it relays it across the mesh.
      */
    type Artifacts =
        BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck | HardAckWithId

    /** What a head peer's own actors hand its mesh liaison: its production, plus the one
      * confirmation its remote-request ceiling is anchored on.
      *
      * [[SoftConfirmedHighWater]] advances `confirmedRemoteRequestHighWater` by max — a block
      * carries only the authors that appear in it, so the notification is advisory and never a
      * cursor. `PeerLiaisonHeadToHead` reads it when it composes the request ceiling of its next
      * [[BatchMessages.Mesh.Get]], which is how a remote peer is stopped from being asked for
      * requests this peer has not confirmed yet.
      */
    type MeshLocal = Artifacts | SoftConfirmedHighWater

    /** What a hub's own actors hand one of its hub↔coil liaisons: its production. A hub serves the
      * population, so it anchors no ceiling of its own.
      */
    type HubLocal = Artifacts

    /** What a coil node's own actors hand its liaison: its own hard-ack to append — the only
      * production a coil peer authors — plus the two confirmations its pull ceilings are anchored
      * on (design/liaison-backpressure.md).
      *
      * Both advance a `Ref` by max, and `PeerLiaisonCoilToHub` reads all three when it composes the
      * four ceilings of its next [[BatchMessages.Population.Get]]:
      *   - [[SoftConfirmedHighWater]] → `softConfirmedBlock` (bounds the block and soft-ack lanes)
      *     and `confirmedRequestHighWater` (bounds each head peer's request lane);
      *   - [[HardConfirmedHighWater]] → `hardConfirmedStack` (bounds the stack and hard-ack lanes).
      *
      * A seeded coil has confirmed nothing of its own yet, so `restoreCeilingAnchors` lifts these
      * to its start point at boot. Leaving them at zero while the cursors sit at the start point
      * deadlocks the link: the hub truncates every lane to nothing and no confirmation ever arrives
      * to raise them.
      */
    type CoilLocal = HardAck | SoftConfirmedHighWater | HardConfirmedHighWater

    // ---- Liaison messages -----------------------------------------------------------------------
    // What each liaison ACTOR accepts, from any source: the ticks it sends itself, what the link
    // brings, and what this node's own actors hand it. Each is named for its actor, since only the
    // link vocabulary travels the link.

    /** A head peer's mesh liaison: serves and pulls one remote head peer's production, and appends
      * its own to its outbox.
      */
    type MeshLiaisonMessage = Control | MeshEmitted | MeshLocal

    /** A hub's liaison to one coil peer: serves that peer's population pull, pulls its own
      * hard-ack, and appends the production `CoilRelay` hands it.
      *
      * [[BatchMessages.Join.Connected]] is local too — the transport synthesizes it from an
      * accepted handshake — and stands on its own line because it is the message that drives this
      * actor's mode change.
      */
    type HubLiaisonMessage = Control | BatchMessages.Join.Connected | CoilEmitted | HubLocal

    /** A coil peer's single liaison to its hub: takes everything its hub may emit, plus what its
      * own actors hand it. [[JoinWaitElapsed]] is coil-only — no other liaison has a join mode to
      * leave.
      */
    type CoilLiaisonMessage = Control | JoinWaitElapsed.type | HubEmitted | CoilLocal

    // ---- Handles --------------------------------------------------------------------------------

    /** ⚠️ Typed at the receiving actor's message union, so it carries `Control` and [[Artifacts]]
      * that never cross a mesh link. [[MeshEmitted]] is the vocabulary it wants (GUM-352).
      */
    type HeadToHeadHandle = ActorRef[IO, MeshLiaisonMessage]

    /** The **coil's** handle to its hub: it carries what a coil may emit, not the hub's inbox. */
    type HubToCoilHandle = ActorRef[IO, CoilEmitted]

    /** The **hub's** handle to one coil peer: it carries what a hub may emit, not the coil's inbox.
      */
    type CoilToHubHandle = ActorRef[IO, HubEmitted]

    /** A coil node's handle to its own liaison, for local appends and notifications. */
    type CoilUplinkHandle = ActorRef[IO, CoilLocal]
}

package hydrozoa.integration.fallbackhandoff

import cats.effect.{IO, Ref}
import cats.implicits.*
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Mutable, CRUD-addressed rule set governing what the harness's per-peer PeerTransport and
  * CardanoBackend drop on outbound paths. Constructed empty by the suite, mutated by the
  * command interpreter (e.g. `IsolatePeerCommand` issues `add`s), and read by the per-channel
  * filter hooks that wrap each peer's transport + backend.
  *
  * `Rule`s name the channel + the targeted peer. The harness threads one `shouldDrop` per
  * channel into each peer's transport/backend; those predicates query the live rule set on
  * every outbound attempt.
  */
final class Firewall private (
    state: Ref[IO, Map[Firewall.RuleId, Firewall.Rule]],
):
    import Firewall.*

    /** Insert or overwrite a rule under `id`. */
    def add(id: RuleId, rule: Rule): IO[Unit] = state.update(_.updated(id, rule))

    /** Drop the rule under `id` if present. */
    def remove(id: RuleId): IO[Unit] = state.update(_.removed(id))

    /** Drop every rule. */
    def clear: IO[Unit] = state.set(Map.empty)

    /** Snapshot the live rule set. */
    def rules: IO[Map[RuleId, Rule]] = state.get

    /** Predicate for the per-peer PeerTransport outbound filter hook. */
    def shouldDropOutboundSig(from: HeadPeerNumber): IO[Boolean] =
        state.get.map(_.values.exists {
            case Rule.DropOutboundPeerSigs(p) => p == from
            case _                            => false
        })

    /** Predicate for the per-peer CardanoBackend outbound-submission filter hook. */
    def shouldDropOutboundTx(from: HeadPeerNumber): IO[Boolean] =
        state.get.map(_.values.exists {
            case Rule.DropOutboundL1Txs(p) => p == from
            case _                         => false
        })

    /** Wire this firewall's per-channel predicates into every peer's PeerTransport + the shared
      * mock CardanoBackend. The hook surface on both sides does not exist yet — this is the
      * TDD target.
      */
    def installInto(harness: MultiPeerHeadHarness.Harness[?, ?]): IO[Unit] = ???

object Firewall:

    /** Caller-supplied stable tag so a generated `remove` can target a prior `add`. */
    opaque type RuleId = String
    object RuleId:
        def apply(s: String): RuleId           = s
        extension (r: RuleId) def value: String = r

    /** What gets dropped, named by channel and targeted peer. Extend (e.g. per-direction,
      * per-message-kind) only when a command actually needs the finer cut.
      */
    enum Rule:
        case DropOutboundPeerSigs(peer: HeadPeerNumber)
        case DropOutboundL1Txs(peer: HeadPeerNumber)

        def shortLabel: String = this match
            case DropOutboundPeerSigs(p) => s"sigs(peer=${p: Int})"
            case DropOutboundL1Txs(p)    => s"tx(peer=${p: Int})"

    /** Empty firewall. The suite mutates it via the command interpreter. */
    def make: IO[Firewall] =
        Ref[IO].of(Map.empty[RuleId, Rule]).map(new Firewall(_))

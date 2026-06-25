package hydrozoa.rulebased.ledger.l1.state

import cats.data.NonEmptyList
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.l1.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus}
import scalus.*
import scalus.cardano.onchain.plutus.prelude.Eq
import scalus.cardano.onchain.plutus.v3.{PubKeyHash, TokenName}
import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins.serialiseData
import scalus.uplc.builtin.Data.{FromData, ToData, toData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}

object VoteDatum {

    /** Seeds the key=0 ballot box directly in the Open phase with versionMinor 0. Any multisigned
      * SEC can ratchet its status forward; the peer signature is not required.
      */
    def public(commitment: KzgCommitment): VoteDatum = VoteState.VoteDatum(
      key = 0,
      // N.B.: Version "Branch: (None) @ c28633a • Commit date: 2025-10-16" of the spec says
      // to set link to `0 < peersN ? 1 : 0`. But we have peers as a NonEmptyList, so this is just 1.
      link = 1,
      voteStatus = VoteStatus.Voted(commitment = commitment, versionMinor = 0)
    )

    // TODO: This should probably also create the default, and should probably be renamed.
    /** Given a non empty list of peers, create a nonempty list of vote datums awaiting votes (one
      * for each peer)
      * @param peers
      * @return
      */
    def apply(peers: NonEmptyList[PubKeyHash]): NonEmptyList[VoteState.VoteDatum] = {
        {
            val numPeers = peers.length
            val mapFunc = (x: (PubKeyHash, Int)) => {
                val i = x._2
                val pkh = x._1
                VoteState.VoteDatum(
                  key = i + 1,
                  link = if i < numPeers - 1 then i + 2 else 0,
                  voteStatus = AwaitingVote(pkh)
                )
            }
            peers.zipWithIndex.map(mapFunc)
        }
    }
}

@Compile
object VoteState:
    // TODO: I'd like to turn this into `VoteDatum[Status <: VoteStatus]`, but then data derivation breaks
    case class VoteDatum(
        // Uniquely identifies a ballot box in the dispute's linked list (rooted at key=0).
        key: Key,
        // References the next ballot box by key, or 0 to indicate end-of-list.
        link: Link,
        voteStatus: VoteStatus
    )

    // Explicit givens (rather than `derives` clauses on the types) so the derived instances are
    // direct members of this `@Compile` object and their SIR is emitted for on-chain use
    // (`inlineDatumOfType[VoteDatum]`, `voteStatus ===` in the rule-based validators). A
    // clause-derived instance lands in the type's companion, whose SIR the on-chain linker cannot
    // resolve (fails at script-build time, not Scala compile time).
    given FromData[VoteDatum] = FromData.derived
    given ToData[VoteDatum] = ToData.derived

    /** Status of a single vote utxo at the dispute resolution address. Maps to the foundation
      * spec's phases: [[AwaitingVote]] is Reserved; [[Voted]] and [[Abstain]] are Open.
      *
      *   - [[AwaitingVote]] — Reserved phase: the named peer can transition this box to [[Voted]]
      *     or [[Abstain]] via the permissioned one-shot path.
      *   - [[Voted]] — Open phase: a KZG commitment + minor block version. Any multisigned SEC with
      *     a strictly higher `versionMinor` (under the same `versionMajor`) can ratchet this status
      *     forward, regardless of who signs the transaction.
      *   - [[Abstain]] — Open phase: the reserved peer opted out (e.g. when no SEC is available
      *     because the latest hard-confirmed stack is Initial or a Major with no trailing minors —
      *     closes the gap left by the KZG-out-of-BlockHeader refactor). A subsequent multisigned
      *     SEC can still ratchet this to [[Voted]] (with any `versionMinor > 0`), enlarging the
      *     open-phase pool.
      */
    enum VoteStatus:
        case AwaitingVote(peer: PubKeyHash)
        case Voted(commitment: KzgCommitment, versionMinor: BigInt)
        case Abstain

    given FromData[VoteStatus] = FromData.derived
    given ToData[VoteStatus] = ToData.derived
    given Eq[VoteStatus] = Eq.derived

    type Key = BigInt

    type Link = BigInt

    // G1 compressed point
    type KzgCommitment = ByteString

    given secFromData: FromData[StandaloneEvacuationCommitmentOnchain] = FromData.derived
    given secToData: ToData[StandaloneEvacuationCommitmentOnchain] = ToData.derived

end VoteState

/** On-chain shape of a standalone evacuation commitment consumed by
  * [[hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator]] as the `sec` field of
  * a [[hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.VoteRedeemer]].
  *
  * Lives in `cardano-onchain` so the validator can reference it without a back-dependency on core.
  * Core keeps `StandaloneEvacuationCommitment.Onchain` as a type alias pointing here.
  */
final case class StandaloneEvacuationCommitmentOnchain(
    headId: TokenName,
    versionMajor: BigInt,
    versionMinor: BigInt,
    commitment: VoteState.KzgCommitment
)

object StandaloneEvacuationCommitmentOnchain:
    import VoteState.secToData

    opaque type Serialized = IArray[Byte]

    def apply(onchainHeader: StandaloneEvacuationCommitmentOnchain): Serialized =
        IArray.from(serialiseData(toData(onchainHeader)).bytes)

    def fromBytes(bytes: Array[Byte]): Serialized = IArray.from(bytes)

    given Conversion[Serialized, IArray[Byte]] = identity

    given Conversion[Serialized, Array[Byte]] = msg => IArray.genericWrapArray(msg).toArray

    given Conversion[Serialized, ByteString] = msg => ByteString.fromArray(msg)

    extension (msg: Serialized) def untagged: IArray[Byte] = identity(msg)

    trait Section:
        def headerSerialized: Serialized

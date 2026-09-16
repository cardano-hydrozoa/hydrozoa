package hydrozoa.multisig.consensus

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.InitializationParameters.HeadId.toHex
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2StateHash}
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import scala.util.control.NonFatal
import scalus.cardano.ledger.rules.{Context, DefaultValidators, OutsideForecastValidator, OutsideValidityIntervalValidator, STS, State, UtxoEnv}
import scalus.cardano.ledger.{CertState, TransactionException}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.platform

/** Why a coil peer refused the start point its hub offered (GUM-312).
  *
  * Every case is a refusal to adopt, never a warning: a coil that cannot establish where its state
  * came from must not start acking over it.
  */
sealed trait JoinRefusal extends RuntimeException

object JoinRefusal:

    /** The settlement is not a transaction the chain would have accepted. `reason` is the scalus
      * validator's own message, which names the rule that failed.
      */
    final case class SettlementInvalid(reason: String) extends JoinRefusal {
        override def getMessage: String = s"offered settlement is not a valid transaction: $reason"
    }

    /** The settlement produces a treasury at an address that is not this head's. */
    final case class WrongTreasuryAddress(expected: String, actual: String) extends JoinRefusal {
        override def getMessage: String =
            s"offered settlement produces a treasury at $actual, not this head's $expected"
    }

    /** The treasury's beacon token is not this head's id — a settlement from a different head
      * instance, possibly one with the very same roster.
      */
    final case class WrongHeadId(expected: String, actual: String) extends JoinRefusal {
        override def getMessage: String =
            s"offered settlement's treasury carries beacon token $actual, not this head's $expected"
    }

    /** The adopted state does not digest to the `l2StateHash` the head peers signed. */
    final case class L2StateMismatch(expected: L2StateHash, actual: L2StateHash)
        extends JoinRefusal {
        override def getMessage: String =
            s"adopted L2 state digests to $actual, but the certificate commits to $expected"
    }

    /** The evacuation map the adopted state projects to is not the one committed on L1. */
    final case class EvacuationMapMismatch(expected: KzgCommitment, actual: KzgCommitment)
        extends JoinRefusal {
        override def getMessage: String =
            s"adopted state's evacuation-map commitment $actual does not match the certified " +
                s"$expected"
    }

    /** The blob came from a ledger this head was not built against. */
    final case class L2ParamsMismatch(expected: String, actual: String) extends JoinRefusal {
        override def getMessage: String =
            s"offered state came from a ledger with parameters $actual, not this head's $expected"
    }

    /** An SEC signature is missing for a head peer, or does not verify. `peerIndex` is the peer's
      * position in the roster.
      */
    final case class SecSignatureInvalid(peerIndex: Int, missing: Boolean) extends JoinRefusal {
        override def getMessage: String =
            if missing then s"offered SEC carries no signature for head peer $peerIndex"
            else s"offered SEC's signature for head peer $peerIndex does not verify"
    }

    /** Fewer than `coilQuorum` coil peers signed the SEC — counting only signatures that are both
      * present and valid.
      *
      * An SEC below quorum is one no head ever hard-confirmed. Distinct from
      * [[SecSignatureInvalid]] because nothing is *wrong* with any individual slot: coil peers sign
      * only while connected, so absent slots are ordinary. There are simply not enough of them.
      */
    final case class SecCoilQuorumNotMet(valid: Int, required: Int) extends JoinRefusal {
        override def getMessage: String =
            s"offered SEC carries $valid valid coil signatures, below the head's quorum of $required"
    }

    /** The settlement and the SEC disagree about the evacuation map. Neither is the donor's fault —
      * the head signed an inconsistent pair — but the coil still cannot tell which to believe.
      */
    final case class CertificateSelfInconsistent(settlement: KzgCommitment, sec: KzgCommitment)
        extends JoinRefusal {
        override def getMessage: String =
            s"the offered settlement commits to evacuation map $settlement while its SEC commits " +
                s"to $sec"
    }

/** What a coil peer checks before it adopts the start point its hub offered (GUM-312).
  *
  * The hub chooses where a coil starts, but it does not get to be believed. Everything here is
  * established against values the coil already holds — its head config, and the digests its **own**
  * ledger reported after importing the state — so a hub that lies is refused rather than obeyed.
  *
  * What this does NOT establish, and cannot:
  *
  *   - **That the settlement is the *latest* major.** A seeded coil has no history, so an older
  *     major's settlement verifies exactly as well as the current one. It surfaces at the first
  *     settlement the coil is asked to sign, whose treasury input it will not hold.
  *   - **That the cursor set matches the start point.** Only that the markers the coil derives are
  *     self-consistent with the cursors it adopted; a coherent lie about position passes.
  */
object JoinOfferVerifier:

    /** Everything the verifier reads: the roster and its multisig address, the head id, the agreed
      * ledger parameters, and the chain parameters the validators need.
      */
    type Config = HeadPeers.Section & InitializationParameters.Section & HeadParameters.Section &
        CardanoNetwork.Section & HeadConfig.Bootstrap.Section

    /** The ledger rules a **historical** settlement must still satisfy: everything scalus checks,
      * minus the two rules that are statements about the chain's clock rather than about the
      * transaction.
      *
      * ⛔ `OutsideValidityIntervalValidator` and `OutsideForecastValidator` come out because a
      * certificate is by construction old — its validity window closed long before any coil asks
      * about it, so both would reject every settlement ever offered. Nothing else is relaxed: the
      * signature rules, `MissingKeyHashesValidator`, and above all `NativeScriptsValidator` — which
      * is what enforces the head multisig's quorum — all apply unchanged.
      */
    val historicalValidators: Set[STS.Validator] =
        DefaultValidators.all
            .filterNot(_.isInstanceOf[OutsideValidityIntervalValidator.type])
            .filterNot(_.isInstanceOf[OutsideForecastValidator.type])

    /** Check an offered start point against what this node already knows.
      *
      * @param settlement
      *   the settlement the treasury comes from — the start point's own when that partition is a
      *   major, otherwise the latest major at or before it.
      * @param sec
      *   the start point's SEC, present exactly when the start point is a minor partition.
      * @param adopted
      *   the digests **this coil's own ledger** reported after importing the offered state. Never
      *   the donor's word for them: a hash that travels with the bytes it describes attests to
      *   nothing.
      */
    def verify(
        settlement: SettlementTx,
        sec: Option[StandaloneEvacuationCommitment.MultiSigned],
        adopted: L2Ledger.Digests
    )(using config: Config): IO[Either[JoinRefusal, Unit]] =
        (for {
            _ <- EitherT.fromEither[IO](checkSettlementValid(settlement))
            _ <- EitherT.fromEither[IO](checkBoundToThisHead(settlement))
            _ <- EitherT.fromEither[IO](checkLedgerParams(adopted))
            _ <- EitherT.fromEither[IO](checkCertificateSelfConsistent(settlement, sec))
            _ <- EitherT.fromEither[IO](checkStateAndMap(settlement, sec, adopted))
            _ <- EitherT(verifySecSignatures(sec))
        } yield ()).value

    /** Public alongside [[verifySecSignatures]], and for the same reason: "would the chain have
      * accepted this transaction, ignoring that its inputs are long spent" is a question worth
      * asking on its own.
      *
      * The settlement must be a transaction the chain would have accepted.
      *
      * **Modulo its inputs already being spent**, which is how a settlement that has been submitted
      * looks from now on: the UTxO state is seeded from the transaction's **own** `resolvedUtxos`
      * rather than resolved against a chain, so `AllInputsMustBeInUtxoValidator` sees exactly the
      * inputs the settlement was built over and nothing is looked up.
      *
      * The slot is zero and unused — the only validators that read it are the two
      * [[historicalValidators]] leaves out.
      */
    def checkSettlementValid(
        settlement: SettlementTx
    )(using config: Config): Either[JoinRefusal, Unit] =
        val context = Context(
          env = UtxoEnv(
            slot = 0L,
            params = config.cardanoProtocolParams,
            certState = CertState.empty,
            network = config.network
          ),
          slotConfig = config.slotConfig
        )
        val state = State(utxos = settlement.resolvedUtxos.utxos)
        STS.Validator
            .validate[TransactionException](historicalValidators, context, state, settlement.tx)
            .leftMap(e => JoinRefusal.SettlementInvalid(Option(e.getMessage).getOrElse(e.toString)))

    /** Two bindings, because they establish different things: the **address** says the settlement
      * belongs to a head with our roster, the **beacon token** says it belongs to *this* head. A
      * sibling head sharing our peers would pass the first and fail the second.
      */
    private def checkBoundToThisHead(
        settlement: SettlementTx
    )(using config: Config): Either[JoinRefusal, Unit] =
        val produced = settlement.treasuryProduced
        for {
            _ <- Either.cond(
              produced.address == config.headMultisigAddress,
              (),
              JoinRefusal.WrongTreasuryAddress(
                config.headMultisigAddress.toBech32.getOrElse(config.headMultisigAddress.toHex),
                produced.address.toBech32.getOrElse(produced.address.toHex)
              )
            )
            // `HeadId` IS the treasury beacon token name (an opaque `AssetName`), so this is a
            // direct comparison rather than a derivation.
            _ <- Either.cond(
              HeadId(produced.treasuryTokenName) == config.headId,
              (),
              JoinRefusal.WrongHeadId(
                config.headId.toHex,
                HeadId(produced.treasuryTokenName).toHex
              )
            )
        } yield ()

    /** The blob must have come from the ledger this head was built against. Free: the coil's own
      * ledger reports this among the digests it computes while importing.
      */
    private def checkLedgerParams(
        adopted: L2Ledger.Digests
    )(using config: Config): Either[JoinRefusal, Unit] =
        Either.cond(
          adopted.l2ParamsHash == config.l2ParamsHash,
          (),
          JoinRefusal.L2ParamsMismatch(config.l2ParamsHash.toHex, adopted.l2ParamsHash.toHex)
        )

    /** When both artifacts are present they must agree about the evacuation map. This catches a
      * head that signed an inconsistent pair rather than a lying donor — but a coil handed two
      * contradictory signed statements cannot pick one, so it refuses.
      */
    private def checkCertificateSelfConsistent(
        settlement: SettlementTx,
        sec: Option[StandaloneEvacuationCommitment.MultiSigned]
    ): Either[JoinRefusal, Unit] =
        sec.fold(Right(())) { s =>
            val settlementCommit = settlement.treasuryProduced.kzgCommitment
            val secCommit = s.commitment.kzgCommitment
            Either.cond(
              settlementCommit == secCommit,
              (),
              JoinRefusal.CertificateSelfInconsistent(settlementCommit, secCommit)
            )
        }

    /** The adopted state must digest to what the head peers signed, in both representations.
      *
      * The **SEC is authoritative when present**: it commits to the start point's own minor, while
      * the settlement is from the latest major at or before it and therefore describes an older
      * state. At a major start point there is no SEC and the settlement's own datum is the one.
      *
      * Both digests are checked even though `l2StateHash` covers the utxo set the map projects from
      * — so a matching state hash nearly implies a matching map. The commitment is the value
      * **anchored on L1**, and checking it is what makes the coil's agreement with the chain
      * explicit rather than transitive.
      */
    private def checkStateAndMap(
        settlement: SettlementTx,
        sec: Option[StandaloneEvacuationCommitment.MultiSigned],
        adopted: L2Ledger.Digests
    ): Either[JoinRefusal, Unit] =
        val (certifiedState, certifiedMap) = sec match {
            case Some(s) => (s.commitment.l2StateHash, s.commitment.kzgCommitment)
            case None =>
                (
                  L2StateHash(settlement.treasuryProduced.datum.l2StateHash),
                  settlement.treasuryProduced.kzgCommitment
                )
        }
        for {
            _ <- Either.cond(
              adopted.l2StateHash == certifiedState,
              (),
              JoinRefusal.L2StateMismatch(certifiedState, adopted.l2StateHash)
            )
            _ <- Either.cond(
              adopted.evacuationMapKzg == certifiedMap,
              (),
              JoinRefusal.EvacuationMapMismatch(certifiedMap, adopted.evacuationMapKzg)
            )
        } yield ()

    /** Public because it answers a question worth asking on its own -- did this SEC reach the
      * quorum a hard-confirmation requires? -- and because nothing else in the tree can answer it:
      * an SEC is not a transaction, so no validator suite covers it.
      *
      * The SEC must carry the same signatures a hard-confirmation required: **every head peer**
      * (AllOf) and **at least `coilQuorum` coil peers** (MOf). Each is checked over the
      * commitment's serialized on-chain record — the message `HardAckSignatureVerifier` uses for a
      * hard-acked SEC.
      *
      * ⛔ The coil half is not optional, and it is the half a settlement gets for free. A settlement
      * is a transaction, so `NativeScriptsValidator` enforces its `AllOf(head) + MOf(coilQuorum,
      * coils)` script for us. An SEC is **not** a transaction — it is a set of header signatures
      * the dispute validator checks — so nothing counts them unless this does. An SEC that never
      * reached coil quorum is one no head ever hard-confirmed.
      *
      * ⚠️ `signatures` is position-aligned over **`allHeadPeers.sorted ++ allCoilPeers.sorted`**,
      * so coil peer `i` sits at `headPeerVKeys.size + i` — never densely packed, because the
      * dispute-resolution script matches `coilMultisig[i]` against `regimeDatum.coilPeers[i]`.
      * Iterating the two groups separately against their own key lists is what keeps a signature
      * checked against the peer that made it.
      *
      * A coil slot may legitimately be `None` — coil peers sign only while connected — so an absent
      * coil signature is not a failure, it simply does not count toward the quorum. A coil
      * signature that is *present and invalid* also does not count: it is worth nothing, whatever
      * it was meant to be.
      */
    def verifySecSignatures(
        sec: Option[StandaloneEvacuationCommitment.MultiSigned]
    )(using config: Config): IO[Either[JoinRefusal, Unit]] =
        sec.fold(IO.pure(Right(()))) { s =>
            val message = s.commitment.header
            val headVKeys = config.headPeerVKeys.toList
            val coilVKeys = config.coilPeerVKeys

            def signatureAt(index: Int): Option[StandaloneEvacuationCommitment.Signature] =
                s.signatures.lift(index).flatten

            def verifies(
                vk: VerificationKey,
                sig: StandaloneEvacuationCommitment.Signature
            ): IO[Boolean] =
                IO.delay(platform.verifyEd25519Signature(vk, message, sig))
                    .handleErrorWith {
                        case NonFatal(_) => IO.pure(false)
                        case e           => IO.raiseError(e)
                    }

            // AllOf over the head prefix: every slot present, every signature good.
            val headChecked: IO[Either[JoinRefusal, Unit]] =
                headVKeys.zipWithIndex
                    .traverse { case (vk, index) =>
                        signatureAt(index) match {
                            case None =>
                                IO.pure(
                                  Left(JoinRefusal.SecSignatureInvalid(index, missing = true))
                                )
                            case Some(sig) =>
                                verifies(vk, sig).map(ok =>
                                    Either.cond(
                                      ok,
                                      (),
                                      JoinRefusal.SecSignatureInvalid(index, missing = false)
                                    )
                                )
                        }
                    }
                    .map(_.sequence.void)

            // MOf over the coil suffix: count only the slots that are present AND verify.
            val coilCounted: IO[Int] =
                coilVKeys.zipWithIndex
                    .traverse { case (vk, coilIndex) =>
                        signatureAt(headVKeys.size + coilIndex)
                            .fold(IO.pure(false))(sig => verifies(vk, sig))
                    }
                    .map(_.count(identity))

            headChecked.flatMap {
                case Left(refusal) => IO.pure(Left(refusal))
                case Right(()) =>
                    coilCounted.map(valid =>
                        Either.cond(
                          valid >= config.coilQuorum,
                          (),
                          JoinRefusal.SecCoilQuorumNotMet(valid, config.coilQuorum)
                        )
                    )
            }
        }

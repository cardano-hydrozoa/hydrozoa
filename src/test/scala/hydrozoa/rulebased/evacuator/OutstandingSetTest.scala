package hydrozoa.rulebased.evacuator

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend.ContinuingTx
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendEvent}
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap, evacuationKeyOrdering}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ShelleyPaymentPart
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.uplc.builtin.Data.toData

/** Checks that the outstanding set is rebuilt from chain history correctly, and — more importantly
  * — that a wrong reconstruction is rejected rather than used.
  *
  * Getting this wrong is not a visible failure: an evacuator that believes the wrong set builds
  * membership proofs against the wrong residual, and every transaction it submits is rejected for
  * reasons that point at the proof rather than at the belief. So the commitment check is the part
  * worth testing hardest.
  */
class OutstandingSetTest extends AnyFunSuite {

    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    private given hydrozoa.config.head.network.CardanoNetwork.Section = env.headConfig

    private val payTo: ShelleyPaymentPart =
        ShelleyPaymentPart.Key(
          env.nodePrivateConfigs.head._2.ownWallet.exportVerificationKey.addrKeyHash
        )

    private val fullMap: EvacuationMap =
        SyntheticMap(40, payTo, env.headConfig.network, Coin.ada(2))
            .fold(v => fail(s"map did not build: $v"), identity)

    private val beacon: (PolicyId, AssetName) =
        (
          env.headConfig.headMultisigScript.policyId,
          env.headConfig.headTokenNames.treasuryTokenName
        )

    private val fallbackTx: TransactionHash =
        TransactionHash.fromHex("00" * 32)

    /** An `Evacuate` redeemer paying out `keys`, as a continuing tx would carry it.
      *
      * The proof is the residual commitment, which is what the validator checks — so it is computed
      * the same way here rather than stubbed, keeping the fixture honest about the shape of a real
      * redeemer.
      */
    private def evacuateTx(
        paid: Set[EvacuationKey],
        remainingBefore: EvacuationMap
    ): ContinuingTx = {
        val subset = EvacuationMap(remainingBefore.evacuationMap.filter((k, _) => paid.contains(k)))
        val proof = Membership
            .mkMembershipProofValidated(set = remainingBefore, subset = subset)
            .fold(e => fail(s"proof failed: $e"), identity)
        val redeemer = TreasuryRedeemer.Evacuate(
          EvacuateRedeemer(
            evacuationKeys = SList.from(subset.evacuationMap.keys.toList),
            proof = proof,
            setupRefInputIdx = BigInt(0)
          )
        )
        ContinuingTx(
          continuingOutput = Utxo(
            TransactionInput(fallbackTx, 0),
            TransactionOutput.Shelley(env.headConfig.headMultisigAddress, Value(Coin.ada(1)), None)
          ),
          spendingRedeemer = redeemer.toData
        )
    }

    /** A backend serving exactly `txs` as the continuing chain. */
    private def backendServing(txs: List[ContinuingTx]): CardanoBackend[IO] =
        new CardanoBackend[IO] {
            override protected def tracer: ContraTracer[IO, CardanoBackendEvent] =
                ContraTracer.nullTracer
            override def resolve(
                input: TransactionInput
            ): IO[Either[CardanoBackend.Error, Option[Utxo]]] = IO.pure(Right(None))
            override def utxosAt(
                address: scalus.cardano.address.ShelleyAddress
            ): IO[Either[CardanoBackend.Error, Map[TransactionInput, TransactionOutput]]] =
                IO.pure(Right(Map.empty))
            override def utxosAt(
                address: scalus.cardano.address.ShelleyAddress,
                asset: (PolicyId, AssetName)
            ): IO[Either[CardanoBackend.Error, Map[TransactionInput, TransactionOutput]]] =
                IO.pure(Right(Map.empty))
            override def isTxKnown(
                txHash: TransactionHash
            ): IO[Either[CardanoBackend.Error, Boolean]] = IO.pure(Right(true))
            override def lastContinuingTxs(
                asset: (PolicyId, AssetName),
                after: TransactionHash
            ): IO[Either[CardanoBackend.Error, List[ContinuingTx]]] = IO.pure(Right(txs))
            override def submitTx(
                etx: hydrozoa.multisig.ledger.l1.tx.EnrichedTx[?]
            ): IO[Either[CardanoBackend.Error, Unit]] = IO.pure(Right(()))
            override def fetchLatestParams: IO[Either[CardanoBackend.Error, ProtocolParams]] =
                IO.pure(Right(env.headConfig.cardanoProtocolParams))
        }

    private def reconstruct(txs: List[ContinuingTx], commitment: String) =
        OutstandingSet
            .reconstruct(backendServing(txs), fullMap, beacon, fallbackTx, commitment)
            .unsafeRunSync()

    test("with nothing evacuated, the outstanding set is the whole map") {
        val result = reconstruct(Nil, fullMap.kzgCommitment.toHex)
        result match {
            case Right(r) =>
                val _ = assert(r.outstanding.evacuationMap == fullMap.evacuationMap)
                assert(r.evacuatedCount == 0)
            case Left(e) => fail(s"reconstruction failed: $e")
        }
    }

    test("keys paid out by past transactions are subtracted") {
        val paid = fullMap.evacuationMap.keys.take(10).toSet
        val residual = fullMap.removedAll(paid)
        val result = reconstruct(List(evacuateTx(paid, fullMap)), residual.kzgCommitment.toHex)

        result match {
            case Right(r) =>
                val _ = assert(r.evacuatedCount == 10)
                val _ = assert(r.outstanding.size == 30)
                assert(r.outstanding.evacuationMap.keySet.intersect(paid).isEmpty)
            case Left(e) => fail(s"reconstruction failed: $e")
        }
    }

    test("a reconstruction that disagrees with the treasury is rejected") {
        // The whole point of the check: subtract the right keys but compare against the wrong
        // commitment, and the result must not be used.
        val paid = fullMap.evacuationMap.keys.take(10).toSet
        val wrong = fullMap.kzgCommitment.toHex // the pre-evacuation commitment, now stale
        reconstruct(List(evacuateTx(paid, fullMap)), wrong) match {
            case Left(_: OutstandingSet.Error.CommitmentMismatch) => succeed
            case other => fail(s"a stale commitment was accepted: $other")
        }
    }

    test("a map that does not belong to this head is rejected") {
        // A key paid out on chain that the map has never held means the preimage is another head's.
        // Catching it here is what stops the bot building proofs against a set the validator will
        // not recognise.
        // Keys are index-derived, so a larger map's tail is exactly a set of keys this map has
        // never held — which is what a foreign preimage looks like from here.
        val beyondRange = SyntheticMap(60, payTo, env.headConfig.network, Coin.ada(2))
            .fold(v => fail(s"map did not build: $v"), identity)
        val strayKeys = beyondRange.evacuationMap.keys.drop(40).take(5).toSet

        val tx = evacuateTx(strayKeys, beyondRange)
        OutstandingSet
            .reconstruct(
              backendServing(List(tx)),
              fullMap,
              beacon,
              fallbackTx,
              fullMap.kzgCommitment.toHex
            )
            .unsafeRunSync() match {
            case Left(OutstandingSet.Error.NotASubset(n)) => assert(n == 5)
            case other => fail(s"keys outside the map were accepted: $other")
        }
    }

    test("a backend failure is reported, not silently treated as nothing evacuated") {
        // The dangerous failure mode: an unreadable history looks exactly like an untouched one, and
        // would have the bot re-evacuate keys that are already paid.
        val failing = new CardanoBackend[IO] {
            override protected def tracer: ContraTracer[IO, CardanoBackendEvent] =
                ContraTracer.nullTracer
            override def resolve(
                input: TransactionInput
            ): IO[Either[CardanoBackend.Error, Option[Utxo]]] = IO.pure(Right(None))
            override def utxosAt(
                address: scalus.cardano.address.ShelleyAddress
            ): IO[Either[CardanoBackend.Error, Map[TransactionInput, TransactionOutput]]] =
                IO.pure(Right(Map.empty))
            override def utxosAt(
                address: scalus.cardano.address.ShelleyAddress,
                asset: (PolicyId, AssetName)
            ): IO[Either[CardanoBackend.Error, Map[TransactionInput, TransactionOutput]]] =
                IO.pure(Right(Map.empty))
            override def isTxKnown(
                txHash: TransactionHash
            ): IO[Either[CardanoBackend.Error, Boolean]] = IO.pure(Right(true))
            override def lastContinuingTxs(
                asset: (PolicyId, AssetName),
                after: TransactionHash
            ): IO[Either[CardanoBackend.Error, List[ContinuingTx]]] =
                IO.pure(Left(CardanoBackend.Error.Timeout("backend unavailable")))
            override def submitTx(
                etx: hydrozoa.multisig.ledger.l1.tx.EnrichedTx[?]
            ): IO[Either[CardanoBackend.Error, Unit]] = IO.pure(Right(()))
            override def fetchLatestParams: IO[Either[CardanoBackend.Error, ProtocolParams]] =
                IO.pure(Right(env.headConfig.cardanoProtocolParams))
        }

        OutstandingSet
            .reconstruct(failing, fullMap, beacon, fallbackTx, fullMap.kzgCommitment.toHex)
            .unsafeRunSync() match {
            case Left(_: OutstandingSet.Error.Backend) => succeed
            case other => fail(s"a backend failure did not surface: $other")
        }
    }
}

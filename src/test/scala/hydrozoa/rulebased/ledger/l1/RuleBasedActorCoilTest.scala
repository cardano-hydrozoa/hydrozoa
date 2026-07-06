package hydrozoa.rulebased.ledger.l1

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.rulebased.ledger.l1.DisputeActorTestHelpers.{coilActorConfig, mkDisputeActor, mkRuleBasedTreasury}
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.Data.{fromData, toData}
import test.Generators.Hydrozoa.genEvacuationMap

/** Coil-peer RBA tests. Reuses [[DisputeActorTestHelpers.mkDisputeActor]] with
  * [[DisputeActorTestHelpers.coilActorConfig]] so the config carries an `OwnCoilPeerPrivate` and
  * `Dispute.handle` routes into `handleCoil`. Uses `runWithCoil(quorum = 0)` so the ratchet VoteTx
  * passes the Plutus coil multisig check with an empty coilSignatures list (the persistence-backed
  * `loadAction` returns coilSignatures = Nil — coil-side recovery deferred).
  */
object RuleBasedActorCoilTest extends Properties("Rule-Based Actor (Coil) Test") {
    import MultiNodeConfig.*

    /** Coil peer sees one Open (Voted) box below the target versionMinor. Its classifier picks the
      * Open box and ratchets it forward with (target.commitment, target.versionMinor).
      */
    def coilRatchetHappyPath: MultiNodeConfigTestM[Boolean] = for {
        env <- ask
        treasuryToken = Value.asset(
          env.headConfig.headMultisigScript.policyId,
          env.headConfig.headTokenNames.treasuryTokenName,
          1
        )
        fallbackTxId <- pick(Arbitrary.arbitrary[TransactionHash])
        nEvacs <- pick(Gen.choose(0, 100))
        evacMap <- pick(genEvacuationMap(nEvacs)(using env))
        versionMajor = 100
        versionMinor = 5
        now <- lift(realTimeQuantizedInstant(env.headConfig.slotConfig))

        ruleBasedTreasury <- mkRuleBasedTreasury(
          versionMajor,
          evacMap.totalValue + treasuryToken,
          TransactionInput(fallbackTxId, 0),
          votingDeadline = now.toPosixTime + 600_000
        )

        // Public (Open) box at key=0, pre-voted with a stale default commitment at
        // versionMinor 0 — below the target versionMinor 5, so ratchet-able.
        staleKzg <- pick(genByteStringOfN(48))
        publicBoxOutput = Babbage(
          address = HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network),
          value = Value.assets(
            lovelace = Coin.ada(5),
            assets = Map(
              (
                env.headConfig.headMultisigScript.policyId,
                Map((env.headConfig.headTokenNames.voteTokenName, 1L))
              )
            )
          ),
          datumOption = Some(
            Inline(
              toData(
                VoteDatum(
                  key = 0,
                  link = 1,
                  voteStatus = Voted(staleKzg, versionMinor = 0)
                )
              )
            )
          ),
          scriptRef = None
        )
        publicBoxInput = TransactionInput(fallbackTxId, env.headConfig.nHeadPeers + 100)

        coilActor <- mkDisputeActor(
          versionMajor = versionMajor,
          versionMinor = versionMinor,
          additionalL1Utxos = Map(
            (
              ruleBasedTreasury.utxoId,
              ruleBasedTreasury.treasuryOutput.toOutput(using env.nodeConfigs.head._2)
            ),
            (publicBoxInput, publicBoxOutput)
          ),
          initialEvacuationMap = evacMap,
          actorConfig = coilActorConfig
        )
        _ <- lift(coilActor.handleTick)

        queryRes <- lift(
          coilActor.cardanoBackend.utxosAt(
            HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network)
          )
        ).flatMap(failLeft)

        _ <- assertWith(
          queryRes.size == 1,
          s"Expected exactly one box at the dispute address post-ratchet, got ${queryRes.size}"
        )

        ratchetted = queryRes.head._2
        _ <- assertWith(
          ratchetted.value == publicBoxOutput.value,
          "Ratchet must preserve the box's value (token + ADA)"
        )
        _ <- assertWith(
          ratchetted.datumOption match {
              case Some(Inline(d)) =>
                  val vd = fromData[VoteDatum](d)
                  vd.key == 0 && vd.link == 1 && vd.voteStatus == Voted(
                    commitment = evacMap.kzgCommitment,
                    versionMinor = versionMinor
                  )
              case _ => false
          },
          "Ratchetted box must carry Voted(target.kzg, target.versionMinor)"
        )
    } yield true

    val _ = property("coil ratchet path") = runWithCoil(nCoil = 5, quorum = 0)(
      coilRatchetHappyPath
    )
}

package hydrozoa.rulebased.ledger.l1

import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{ArrivalStamp, InMemoryBackendStore, Persistence, PersistenceEventFormat, StoreKey, Timestamped}
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import hydrozoa.rulebased.{RuleBasedActor, RuleBasedActorEventFormat}
import org.scalacheck.{Gen, Properties}
import test.Generators.Hydrozoa.genEvacuationMap

/** Regression test for the evacuation candidate-map derivation. When a head falls into the
  * rule-based regime with only an Initial stack plus minor-only Regular stacks (no major has
  * settled — e.g. one L2 transaction submitted, then the fallback fires), voting can resolve the
  * dispute onto a **minor SEC** ([[RuleBasedActor.votableSecs]]). Evacuation must then hold a
  * preimage for that resolved commitment: [[RuleBasedActor.loadEvacuationInputs]] must include
  * every votable SEC's map in `candidateEvacMaps`, not just the initial map. A regression here
  * surfaces live as `UnknownResolvedKzg` (the resolved minor-SEC kzg is not in the loaded set).
  */
object EvacuationInputsTest extends Properties("Evacuation Inputs Test") {

    import DisputeActorTestHelpers.*
    import MultiNodeConfig.*

    /** Seed a store with `Initial` at stack 0 (from the head config's init/fallback txs) and a
      * minor-only `Regular` at stack 1 carrying a SEC that commits to `minorMap` at `versionMajor`,
      * plus the block's `EvacuationMap`. This is the "one L2 tx, then fallback" shape: the walk
      * from stack 1 finds no fallback there and terminates on the Initial stack.
      */
    def mkPersistenceInitialPlusMinorSec(
        env: MultiNodeConfig,
        minorMap: EvacuationMap,
        versionMajor: BigInt,
        versionMinor: BigInt,
    ): IO[Persistence[IO]] =
        val blockHeader = StandaloneEvacuationCommitmentOnchain(
          headId = env.headConfig.headTokenNames.treasuryTokenName.bytes,
          versionMajor = versionMajor,
          versionMinor = versionMinor,
          commitment = minorMap.kzgCommitment
        )
        val multiSigned = StandaloneEvacuationCommitment.MultiSigned(
          commitment = StandaloneEvacuationCommitment(
            blockNum = BlockNumber(1),
            blockVersion = BlockVersion.Full(versionMajor.toInt, versionMinor.toInt),
            kzgCommitment = minorMap.kzgCommitment,
            header = StandaloneEvacuationCommitmentOnchain(blockHeader),
          ),
          headerMultiSigned = env.multisignHeaderSparse(blockHeader),
        )
        val regularStack = StackEffects.HardConfirmed.Regular(
          NonEmptyList.of(PartitionEffects.Minor(sec = multiSigned, refunds = List.empty))
        )
        val initEffects = env.headConfig.initialBlock.effects
        val initialStack = StackEffects.HardConfirmed.Initial(
          initializationTx = initEffects.initializationTx,
          fallbackTx = initEffects.fallbackTx
        )
        val tracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        for
            backend <- InMemoryBackendStore.open(tracer).allocated.map(_._1)
            persistence <- Persistence.fromBackend(backend, tracer)(using env.headConfig)
            _ <- persistence.put(StoreKey.HardConfirmation(StackNumber.zero))(
              Timestamped(ArrivalStamp(0, 0L), initialStack)
            )
            _ <- persistence.put(StoreKey.HardConfirmation(StackNumber.first))(
              Timestamped(ArrivalStamp(0, 1L), regularStack)
            )
            _ <- persistence.put(StoreKey.EvacuationMap(BlockNumber(1)))(minorMap)
        yield persistence

    /** With an Initial + minor-only history whose dispute resolves onto the minor SEC,
      * `loadEvacuationInputs` must hold a preimage for that SEC's kzg.
      */
    def candidateMapsIncludeVotableMinorSec: MultiNodeConfigTestM[Boolean] = for {
        env <- ask
        nEvacs <- pick(Gen.choose(1, 50))
        // The minor block's map must differ from the initial map's commitment, otherwise the
        // presence of the initial map alone would satisfy the check vacuously.
        minorMap <- pick(
          genEvacuationMap(nEvacs)(using env)
              .suchThat(_.kzgCommitment != env.headConfig.initialEvacuationMap.kzgCommitment)
        )
        versionMajor = BigInt(0)
        versionMinor = BigInt(1)

        persistence <- lift(
          mkPersistenceInitialPlusMinorSec(env, minorMap, versionMajor, versionMinor)
        )
        actorNodeConfig <- defaultActorConfig
        // `loadEvacuationInputs` reads only persistence + config; the backend is unused here.
        cardanoBackend <- lift(CardanoBackendMock.mockIO(MockState(initialUtxos = Map.empty)))
        tracer = Slf4jTracer.sink.contramap(
          RuleBasedActorEventFormat.humanFormat(HeadPeerNumber(0))
        )
        actor = RuleBasedActor(persistence, cardanoBackend, tracer)(using actorNodeConfig)

        inputs <- lift(actor.loadEvacuationInputs(versionMajor))
        _ <- assertWith(
          inputs.candidateEvacMaps.contains(minorMap.kzgCommitment),
          "loadEvacuationInputs must hold a preimage for the resolvable minor SEC's kzg " +
              s"${minorMap.kzgCommitment}, but the loaded candidate set is " +
              s"${inputs.candidateEvacMaps.keySet}"
        )
    } yield true

    val _ = property("candidate maps include a votable minor SEC (Initial + minor-only history)") =
        runDefault(candidateMapsIncludeVotableMinorSec)
}

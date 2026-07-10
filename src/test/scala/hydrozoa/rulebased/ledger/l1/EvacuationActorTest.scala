package hydrozoa.rulebased.ledger.l1

import cats.*
import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.config.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.persistence.{InMemoryBackendStore, Persistence, PersistenceEventFormat}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
import hydrozoa.rulebased.{RuleBasedActor, RuleBasedActorEventFormat}
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyM}
import scala.collection.immutable.TreeMap
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.rules.State
import test.Generators.Hydrozoa.genEvacuationMap
import test.{TestM, TestPeersSpec}

//class TestTransaction(
//    val body: KeepRaw[TransactionBody],
//    val witnessSetRaw: KeepRaw[TransactionWitnessSet],
//    val auxiliaryData: Option[KeepRaw[AuxiliaryData]],
//    override val id: TransactionHash) extends Transaction(body, KeepRaw[TransactionWitnessSet.empty], true, None)

object EvacuationActorTestHelpers {
    export MultiNodeConfig.*

    def mkResolutionTx: MultiNodeConfigTestM[Transaction] = ???

    def mkEvacuationActor: MultiNodeConfigTestM[RuleBasedActor] =
        for {
            env <- ask
            nEvacs <- pick(Gen.choose(0, 1000))
            evacMapFull <- pick(
              genEvacuationMap(nEvacs)(using env)
                  .label("Full evacuation map at fallback")
            )
            // Also might be empty, even if the full map isn't
            _ <- pick(
              Gen.someOf(evacMapFull.evacuationMap)
                  .flatMap(s => EvacuationMap(TreeMap.from(s)))
                  .label("subset toEvacuate")
            )
            fallbackTxHash <- pick(Arbitrary.arbitrary[TransactionHash].label("Fallback tx hash"))

            resolutionTx <- mkResolutionTx

            cardanoBackend <- lift(
              CardanoBackendMock.mockIO(
                MockState(
                  ledgerState = State(),
                  currentSlot = Slot(0L),
                  knownTxs = Set(fallbackTxHash, resolutionTx.id),
                  submittedTxs = List.empty
                )
              )
            )

            tracer = Slf4jTracer.sink.contramap(
              RuleBasedActorEventFormat.humanFormat(env.nodeConfigs.head._1)
            )

            // The test body doesn't actually invoke the actor, so an empty persistence is
            // sufficient — loadAction / loadEvacuationInputs would raise `MissingState` on any
            // real read (no hard-confirmed stack on disk). Keeps the fixture minimal.
            persistence <- lift(mkEmptyPersistence(env))
        } yield RuleBasedActor(
          persistence = persistence,
          cardanoBackend = cardanoBackend,
          tracer = tracer
        )(using
          env.nodeConfigs.head._2
        )

    private def mkEmptyPersistence(env: MultiNodeConfig): IO[Persistence[IO]] =
        val persistenceTracer =
            Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        for
            backend <- InMemoryBackendStore.open(persistenceTracer).allocated.map(_._1)
            persistence <- Persistence.fromBackend(backend, persistenceTracer)(using
              env.headConfig
            )
        yield persistence

}

object EvacuationActorTest extends Properties("Evacuation Actor") {

    import EvacuationActorTestHelpers.*

    val _ = property("dispute actor (no actor system)") = run(
      resource = PropertyM
          .pick[IO, MultiNodeConfig](MultiNodeConfig.generate(TestPeersSpec.default)())
          .map(Resource.pure[IO, MultiNodeConfig](_)),
      testM = for {
          _ <- ask
      } yield true
    )
}

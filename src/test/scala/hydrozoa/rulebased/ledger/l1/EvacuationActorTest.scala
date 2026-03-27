package hydrozoa.rulebased.ledger.l1

import cats.*
import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.rulebased.EvacuationActor
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
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

    def mkEvacuationActor: MultiNodeConfigTestM[EvacuationActor] =
        for {
            env <- ask
            nEvacs <- pick(Gen.choose(0, 1000))
            evacMapFull <- pick(
              genEvacuationMap(nEvacs)(using env)
                  .label("Full evacuation map at fallback")
            )
            // Also might be empty, even if the full map isn't
            subset <- pick(
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

        } yield EvacuationActor(
          env.nodeConfigs.head._2
        )(
          toEvacuate = subset,
          cardanoBackend = cardanoBackend,
          evacuationMapAtFallback = evacMapFull,
          fallbackTxHash = fallbackTxHash
        )

}

object EvacuationActorTest extends Properties("Evacuation Actor") {

    import EvacuationActorTestHelpers.*

    val _ = property("dispute actor (no actor system)") = run(
      initializer = PropertyM.pick(MultiNodeConfig.generate(TestPeersSpec.default)()),
      testM = for {
          env <- ask
      } yield true
    )
}

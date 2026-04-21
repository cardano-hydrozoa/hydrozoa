package hydrozoa.integration.stage1

import hydrozoa.config.head.multisig.timing.{generateDefaultTxTiming, generateTestnetTxTiming, generateYaciTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.stage1.ScenarioGenerators.*
import hydrozoa.integration.stage1.Stage1PropertiesL1Mock.property
import hydrozoa.integration.stage1.SuiteCardano.{Mock, Public, Yaci}
import hydrozoa.integration.yaci.DevKit
import org.scalacheck.YetAnotherProperties
import test.SeedPhrase

object Stage1PropertiesL1Mock extends YetAnotherProperties("Integration Stage 1 on L1 mock"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1)
        // NB: careful, this will override -s from the command line
        // .withMinSuccessfulTests(100) // 10000
        // .withMaxSize(100) // 500
    }

    private val preprod = CardanoNetwork.Preprod

    /** Block promotion
      *
      * This property checks that block promotion Minor -> Major works correctly. It uses
      * [[NoWithdrawalsScenarioGen]] which produces L2 transactions with no withdrawals. L2 events
      * not strictly needed for testing block promotion, which must work on empty blocks, but we
      * additionally decided to check block brief at the same time.
      */
    val _ = property("Block promotion with real L2 txs") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      scenarioGen = NoWithdrawalsScenarioGen,
      label = "block-promotion-mock"
    ).property()

    /** Dusty head finalization
      *
      * This scenario leverages the fact that all utxos should be evacuated upon head finalization.
      * It continues splitting up big utxos in L2 till it reaches the desired level of fragmentation
      * and once the target is hit finalizes the head immediately. Only command sequences that
      * satisfy the condition "head is finalized" are run.
      */
    lazy val _ = property("Dusty head finalization") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      scenarioGen = MakeDustScenarioGen(minL2Utxos = 500),
      label = "dusty-finalization-mock"
    ).property()

    /** Ongoing withdrawals
      *
      * This scenario test that settlement txs can actually withdraw funds.
      *
      * TODO: do we want to test the rollout sequence with in the settlement tx seq specifically?
      */
    lazy val _ = property("Ongoing withdrawals") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      scenarioGen = OngoingWithdrawalsScenarioGen,
      label = "ongoing-withdrawals-mock"
    ).property()

    /** Deposits
      *
      * This scenario brings up deposits to the scene by adding two additional commands:
      *   - [[RegisterDepositCommand]]
      *   - [[SubmitDepositCommand]]
      */
    lazy val _ = property("Deposits") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      scenarioGen = DepositsScenarioGen,
      label = "deposits-mock"
    ).property()

/** The Yaci runner has only some of the properties which are worth running on Yaci, see property
  * descriptions in the [[Stage1PropertiesL1Mock]]. To run this suite you need a Yaci devkit up and
  * running.
  */
object Stage1PropertiesYaci extends YetAnotherProperties("Integration Stage 1 with Yaci"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1)
            .withMinSuccessfulTests(1)
    }

    lazy val _ = property("Block promotion Yaci") = Suite(
      suiteCardano = Yaci(
        protocolParams = DevKit.yaciParams
      ),
      txTimingGen = generateYaciTxTiming,
      scenarioGen = NoWithdrawalsScenarioGen,
      label = "block-promotion-yaci"
    ).property()

    lazy val _ = property("Dusty head finalization Yaci") = Suite(
      suiteCardano = Yaci(
        protocolParams = DevKit.yaciParams
      ),
      txTimingGen = generateYaciTxTiming,
      scenarioGen = MakeDustScenarioGen(minL2Utxos = 500),
      label = "dusty-finalization-yaci"
    ).property()

    lazy val _ = property("Deposits on Yaci") = Suite(
      suiteCardano = Yaci(
        protocolParams = DevKit.yaciParams
      ),
      txTimingGen = generateYaciTxTiming,
      scenarioGen = DepositsScenarioGen,
      label = "deposits-yaci"
    ).property()

/** TODO:
  */
object Stage1PropertiesPublic extends YetAnotherProperties("Integration Stage 1 with Preview"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1)
            .withMinSuccessfulTests(1)
    }

     lazy val _ = property("Block promotion Yaci") = Suite(
        suiteCardano = Yaci(
            protocolParams = DevKit.yaciParams
        ),
        txTimingGen = generateYaciTxTiming,
        scenarioGen = NoWithdrawalsScenarioGen,
        label = "block-promotion-public"
     ).property()

     lazy val _ = property("Dusty head finalization Yaci") = Suite(
        suiteCardano = Yaci(
            protocolParams = DevKit.yaciParams
        ),
        txTimingGen = generateYaciTxTiming,
        scenarioGen = MakeDustScenarioGen(minL2Utxos = 500),
        label = "dusty-finalization-public"
     ).property()

    lazy val _ = property("Deposits on Preview") = Suite(
      suiteCardano = Public(
        cardanoNetwork = CardanoNetwork.Preview,
        blockfrostKey = "previewQQFamFAznFQgz0uRG9OntxgqJczreq9z",
        seedPhrase = SeedPhrase.Public
      ),
      txTimingGen = generateTestnetTxTiming,
      scenarioGen = DepositsScenarioGen,
      label = "deposits-public"
    ).property()

package hydrozoa.integration.stage1

import hydrozoa.config.head.multisig.timing.{generateDefaultTxTiming, generateYaciTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.stage1.Stage1PropertiesL1Mock.property
import hydrozoa.integration.stage1.SuiteCardano.{Mock, Yaci}
import hydrozoa.integration.yaci.DevKit
import hydrozoa.multisig.backend.cardano.yaciTestSauceGenesis
import org.scalacheck.YetAnotherProperties

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
      * This property checks that block promotion Minor -> Major * works correctly. It uses
      * [[NoWithdrawalsCommandGen]] which produces L2 transactions with no withdrawals. L2 events
      * not strictly needed for testing block promotion, which must work on empty blocks, but we
      * additionally decided to check block brief at the same time.
      */
    val _ = property("Block promotion with real L2 txs") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = NoWithdrawalsCommandGen
    ).property()

    /** Dusty head finalization
      *
      * This scenario leverages the fact that all utxos should be evacuated upon head finalization.
      * It continues splitting up big utxos in L2 till it reaches the desired level of fragmentation
      * and once the target is hit finalizes the head immediately. Only command sequences that
      * satisfy the condition "head is finalized" are run.
      */
    val _ = property("Dusty head finalization") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = MakeDustCommandGen(minL2Utxos = 500)
    ).property()

    /** Ongoing withdrawals
      *
      * This scenario test that settlement txs can actually withdraw funds.
      *
      * TODO: do we want to test the rollout sequence with in the settlement tx seq specifically?
      */
    val _ = property("Ongoing withdrawals") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = OngoingWithdrawalsCommandGen
    ).property()

    /** Deposits
      *
      * TODO:
      */
    lazy val _ = property("Deposits") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = DepositsCommandGen
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

    private val preprod = CardanoNetwork.Preprod

    val _ = property("Block promotion Yaci") = Suite(
      suiteCardano = Yaci(
        protocolParams = DevKit.yaciParams
      ),
      txTimingGen = generateYaciTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = NoWithdrawalsCommandGen
    ).property()

    val _ = property("Dusty head finalization Yaci") = Suite(
      suiteCardano = Yaci(
        protocolParams = DevKit.yaciParams
      ),
      txTimingGen = generateYaciTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = MakeDustCommandGen(minL2Utxos = 500)
    ).property()

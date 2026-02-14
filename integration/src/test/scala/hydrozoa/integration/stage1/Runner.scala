package hydrozoa.integration.stage1

import hydrozoa.config.head.multisig.timing.{generateDefaultTxTiming, generateYaciTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.stage1.Stage1PropertiesL1Mock.property
import hydrozoa.integration.stage1.SuiteCardano.{Mock, Yaci}
import hydrozoa.integration.yaci.DevKit
import hydrozoa.multisig.backend.cardano.yaciTestSauceGenesis
import org.scalacheck.YetAnotherProperties

object Stage1PropertiesL1Mock extends YetAnotherProperties("Integration Stage 1 with L1 mock"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1)
            .withMinSuccessfulTests(100) // 10000
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
    val _ = property("Block promotion with arbitrary events L1 mock") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = ArbitraryL2EventsCommandGen
    ).property()

    lazy val _ = property("Block promotion L1 mock") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = NoWithdrawalsCommandGen
    ).property()

    /** Withdrawals onslaught
      *
      * TODO:
      */
    lazy val _ = property("Dusty head finalization L1 mock") = Suite(
      suiteCardano = Mock(preprod),
      txTimingGen = generateDefaultTxTiming,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.network),
      commandGen = MakeDustCommandGen(minL2Utxos = 500)
    ).property()

object Stage1PropertiesYaci extends YetAnotherProperties("Integration Stage 1 with Yaci"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1)
            .withMinSuccessfulTests(3)
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

package hydrozoa.integration.stage1

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import cats.syntax.applicative.*
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.HeadConfig.OwnPeer
import hydrozoa.config.RawConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, quantize}
import hydrozoa.multisig.backend.cardano.yaciTestSauceGenesis
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.{TxTiming, minInitTreasuryAda}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq.Builder
import hydrozoa.multisig.protocol.types.Peer
import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
import hydrozoa.{Address, L1, UtxoSetL1, attachVKeyWitnesses, maxNonPlutusTxFee}
import org.scalacheck.commands.YetAnotherCommands
import org.scalacheck.{Gen, Properties}
import scala.concurrent.duration.{DurationInt, FiniteDuration, HOURS}
import scalus.cardano.address.Network
import scalus.cardano.address.Network.Testnet
import scalus.cardano.ledger.{CardanoInfo, Coin, ProtocolParams, SlotConfig, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import spire.math.UByte
import test.TestPeer.Alice
import test.{TestPeer, minPubkeyAda, sumUtxoValues, testTxBuilderCardanoInfo}

/** Stage 1 (the simplest).
  *   - Only two real actors are involved: [[JointLedger]] and [[CardanoLiaison]]
  */
object Stage1Properties
//extends YetAnotherProperties("Joint ledger and Cardano liaison (stage 1"):
    extends Properties("Joint ledger and Cardano liaison (stage 1"):

    val _ = property("Work well on L1 mock (fast, reproducible)") = Stage1(
      network = Testnet,
      slotConfig = testTxBuilderCardanoInfo.slotConfig,
      protocolParams = testTxBuilderCardanoInfo.protocolParams,
      genesisUtxo = yaciTestSauceGenesis(Testnet)
    ).property0()
// val _ = property("Work well on Yaci DevKit (slow, reproducible)") = ???
// val _ = property("Work well on Preview (slow, non-reproducible)") = ???

case class Stage1(
    network: Network,
    slotConfig: SlotConfig,
    protocolParams: ProtocolParams,
    genesisUtxo: Map[TestPeer, UtxoSetL1]
) extends YetAnotherCommands {

    override type State = HarnessState
    override type Sut = Stage1Sut

    // ===================================
    // SUT handling
    // ===================================

    override def newSut(state: HarnessState): Sut = ???

    override def destroySut(sut: Sut): Unit = ???

    // TODO: do we want to run multiple SUTs?
    override def canCreateNewSut(
        _newState: State,
        initSuts: Iterable[State],
        runningSuts: Iterable[Sut]
    ): Boolean = initSuts.isEmpty && runningSuts.isEmpty // only one SUT is allowed

    // ===================================
    // Initial state handling
    // ===================================

    override def initialPreCondition(state: HarnessState): Boolean = true

    override def genInitialState: Gen[State] = for {
        startTime <- Gen
            .const(realTimeQuantizedInstant(slotConfig).unsafeRunSync())
            .label("Zero block creation time")

        ownTestPeer = Alice
        peers = NonEmptyList.one(ownTestPeer) // useful to have since many gens want it
        ownPeerIndex = 0
        equityShares <- genEquityShares(peers, network).label("Equity shares")
        ownPeer = (
          OwnPeer(Peer.Id(ownPeerIndex, peers.size), ownTestPeer.wallet),
          Address[L1](ownTestPeer.address()),
          equityShares.peerShares(UByte(ownPeerIndex)).equityShare
        )

        seedUtxo <- Gen
            .oneOf(
              genesisUtxo(ownTestPeer)
                  .map(u => Utxo(u._1.untagged, u._2.untagged))
            )
            .label("seed utxo")

        // NB: I don't think funding utxos make sense for this scenario
        spentUtxos = NonEmptyList.one(seedUtxo)

        // Initial treasury must be at least enough for the minAda of the treasury, and no more than the
        // sum of the seed+funding utxos, while leaving enough left for the estimated fee and the minAda of the change
        // output
        initialTreasuryCoin <- Gen
            .choose(
              minInitTreasuryAda.value,
              sumUtxoValues(spentUtxos.toList).coin.value
                  - maxNonPlutusTxFee(protocolParams).value
                  - minPubkeyAda().value
            )
            .map(Coin(_))
            .label("initial treasury coin")

        initialTreasury = Value(initialTreasuryCoin)

        txTiming = TxTiming.default(slotConfig)

        initTxConfig = InitializationTxSeq.Config(
          tallyFeeAllowance = Coin.ada(2),
          votingDuration = FiniteDuration(24, HOURS).quantize(slotConfig),
          cardanoInfo = CardanoInfo(
            network = network,
            slotConfig = slotConfig,
            protocolParams = protocolParams
          ),
          peerKeys = peers.map(_.wallet.exportVerificationKeyBytes),
          startTime = startTime,
          txTiming = txTiming
        )

        initTxArgs =
            InitializationTxSeq.Builder.Args(
              spentUtxos = SpentUtxos(seedUtxo, List.empty),
              initialTreasury = initialTreasury,
              initializationTxChangePP = ownTestPeer.address().payment,
            )

        headMultisigScript = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))

        initTxSeq <- InitializationTxSeq.Builder
            .build(initTxArgs, initTxConfig)
            .fold(
              // TODO: this should be unified in the builder
              err => {
                  err match {
                      case Builder.FallbackPRError(e)       => println(e)
                      case Builder.InitializationTxError(e) => println(e)
                      case Builder.FallbackTxError(e)       => println(e)
                  }
                  Gen.fail
              },
              Gen.const
            )

        initTxSigned = attachVKeyWitnesses(
          initTxSeq.initializationTx.tx,
          List(ownTestPeer.wallet.signTx(initTxSeq.initializationTx.tx))
        )
        fallbackTxSigned = attachVKeyWitnesses(
          initTxSeq.fallbackTx.tx,
          List(ownTestPeer.wallet.signTx(initTxSeq.fallbackTx.tx))
        )

        rawConfig: RawConfig = RawConfig(
          ownPeer = ownPeer,
          otherPeers = List.empty,
          receiveTimeout = 10.seconds,
          initializationTxBytes = initTxSigned.toCbor,
          initialFallbackTxBytes = fallbackTxSigned.toCbor,
          network = network,
          tallyFeeAllowance = Coin.ada(2),
          votingDuration = QuantizedFiniteDuration(
            finiteDuration = 24.hours,
            slotConfig = testTxBuilderCardanoInfo.slotConfig
          ),
          txTiming = txTiming,
          startTime = startTime,
          resolvedUtxosForInitialization =
              ResolvedUtxos(Map.from(spentUtxos.toList.map(_.toTuple))),
          // Can it be the peer's automated wallet though, at least for now?
          withdrawalFeeWallet = ownTestPeer.wallet,
          pollingPeriod = 5.seconds
        )

        _ = println(rawConfig)
        _ = println(HexUtil.encodeHexString(rawConfig.initializationTxBytes))

    } yield HarnessState(headConfig = ???)

    // ===================================
    // Command generation
    // ===================================

    override def genCommand(state: State): Gen[Command0] = ???

}

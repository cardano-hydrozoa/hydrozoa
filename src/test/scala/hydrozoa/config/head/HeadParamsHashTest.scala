package hydrozoa.config.head

import cats.data.Validated
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.block.BlockConfig
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.parameters.{HeadParameters, L2LedgerKind, RateLimits}
import hydrozoa.config.head.peers.{HeadPeerData, HeadPeers}
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.number.PositiveInt
import org.http4s.Uri
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Prop, Properties}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash}
import scalus.uplc.builtin.ByteString
import test.{TestPeers, TestPeersSpec}

/** [[HeadParamsHash]] must be stable for a given config and must move when any covered field moves.
  *
  * A field that silently falls out of the preimage is invisible in production — every peer simply
  * agrees on a hash that does not constrain the thing that differs — so each covered field gets its
  * own mutation here. [[hydrozoa.config.head.peers.HeadPeerData.webSocketAddress]] gets the
  * opposite check: it is deliberately excluded, and folding it in would make every peer relocation
  * a head re-initialization.
  */
object HeadParamsHashTest extends Properties("HeadParamsHash") {

    private val generateConfig =
        TestPeersSpec.generate().flatMap(TestPeers.generate).flatMap(generateHeadConfig().run(_))

    /** Rebuild a config with `headParameters` replaced, keeping everything else identical. */
    private def withParameters(hc: HeadConfig, params: HeadParameters): HeadConfig =
        rebuild(hc, params, hc.headConfigBootstrap.initializationParameters, hc.headPeers)

    private def rebuild(
        hc: HeadConfig,
        params: HeadParameters,
        initParams: InitializationParameters,
        headPeers: HeadPeers
    ): HeadConfig = {
        val bootstrap = HeadConfig.Bootstrap(
          cardanoNetwork = hc.cardanoNetwork,
          headParams = params,
          headPeers = headPeers,
          coilPeers = hc.coilPeers,
          initializationParams = initParams,
          scriptReferenceUtxos = hc.scriptReferenceUtxos
        ) match {
            case Validated.Valid(b)   => b
            case Validated.Invalid(e) => throw RuntimeException(s"bootstrap rebuild failed: $e")
        }
        HeadConfig(bootstrap, hc.initialBlockSection) match {
            case Validated.Valid(c)   => c
            case Validated.Invalid(e) => throw RuntimeException(s"config rebuild failed: $e")
        }
    }

    /** One mutation per covered [[HeadParameters]] field. `txTiming` is absent because its
      * constructor is private — it gets its own whole-section swap below.
      */
    private val parameterMutations: List[(String, HeadParameters => HeadParameters)] = List(
      "rateLimits.softBlockMinPeriod" -> (p =>
          p.copy(rateLimits = p.rateLimits.copy(softBlockMinPeriod = 7777.millis))
      ),
      "rateLimits.hardStackMinPeriod" -> (p =>
          p.copy(rateLimits = p.rateLimits.copy(hardStackMinPeriod = 8888.millis))
      ),
      "l2ParamsHash" -> (p =>
          p.copy(l2ParamsHash =
              Hash[Blake2b_256, Any](ByteString.fromArray(Array.fill[Byte](32)(0x5a)))
          )
      ),
      "l2Ledger" -> (p =>
          p.copy(l2Ledger =
              if p.l2Ledger == L2LedgerKind.CardanoEutxo then L2LedgerKind.AnyRemote
              else L2LedgerKind.CardanoEutxo
          )
      ),
      "identityIsomorphism" -> (p => p.copy(identityIsomorphism = !p.identityIsomorphism)),
      "coilQuorum" -> (p => p.copy(coilQuorum = p.coilQuorum + 1)),
      "settlementConfig.maxDepositsAbsorbedPerBlock" -> (p =>
          p.copy(settlementConfig =
              SettlementConfig(bump(p.settlementConfig.maxDepositsAbsorbedPerBlock))
          )
      ),
      "blockConfig.maxRequestsPerBlock" -> (p =>
          p.copy(blockConfig =
              p.blockConfig.copy(maxRequestsPerBlock = bump(p.maxRequestsPerBlock))
          )
      ),
      "blockConfig.backpressureCoefficient" -> (p =>
          p.copy(blockConfig =
              p.blockConfig.copy(backpressureCoefficient = bump(p.backpressureCoefficient))
          )
      ),
      "fallbackContingency.collective" -> (p =>
          p.copy(fallbackContingency =
              p.fallbackContingency.copy(collectiveContingency =
                  p.collectiveContingency
                      .copy(fallbackTxFee = Coin(p.collectiveContingency.fallbackTxFee.value + 1))
              )
          )
      ),
      "fallbackContingency.individual" -> (p =>
          p.copy(fallbackContingency =
              p.fallbackContingency.copy(individualContingency =
                  p.individualContingency
                      .copy(voteDeposit = Coin(p.individualContingency.voteDeposit.value + 1))
              )
          )
      ),
      "disputeResolutionConfig.votingDuration" -> (p =>
          p.copy(disputeResolutionConfig =
              DisputeResolutionConfig(p.votingDuration + p.votingDuration)
          )
      )
    )

    private def bump(n: PositiveInt): PositiveInt = PositiveInt.unsafeApply(n.convert + 1)

    val _ = property("is deterministic") = Prop.forAll(generateConfig) { hc =>
        hc.headParamsHash == HeadParamsHash(
          hc.headConfigBootstrap,
          hc.initialBlock.blockBrief.header
        )
    }

    /** `txTiming`'s constructor is private, so it cannot be mutated field by field like the rest.
      * Swapping the whole section for [[TxTiming.demo]] covers it instead.
      */
    val _ = property("covers txTiming") = Prop.forAll(generateConfig) { hc =>
        val demo = TxTiming.demo(hc.cardanoNetwork.slotConfig)
        (demo != hc.txTiming) ==> {
            withParameters(hc, hc.headParameters.copy(txTiming = demo)).headParamsHash
                != hc.headParamsHash
        }
    }

    parameterMutations.foreach { (name, mutate) =>
        val _ = property(s"covers $name") = Prop.forAll(generateConfig) { hc =>
            val mutated = withParameters(hc, mutate(hc.headParameters))
            mutated.headParamsHash != hc.headParamsHash
        }
    }

    val _ = property("covers initialEquityContributions") = Prop.forAll(generateConfig) { hc =>
        val initParams = hc.headConfigBootstrap.initializationParameters
        val (peer, coin) = initParams.initialEquityContributions.toSortedMap.head
        val mutated = rebuild(
          hc,
          hc.headParameters,
          initParams.copy(initialEquityContributions =
              initParams.initialEquityContributions.add(peer -> Coin(coin.value + 1))
          ),
          hc.headPeers
        )
        mutated.headParamsHash != hc.headParamsHash
    }

    val _ = property("excludes webSocketAddress") = Prop.forAll(generateConfig) { hc =>
        val data = hc.headPeers.headPeerData
        val (peer, peerData) = data.toSortedMap.head
        val moved = HeadPeers(
          data.add(
            peer -> HeadPeerData(
              peerData.verificationKey,
              Uri.unsafeFromString("ws://relocated.example:9999")
            )
          )
        ).getOrElse(throw RuntimeException("head peers rebuild failed"))
        val mutated = rebuild(
          hc,
          hc.headParameters,
          hc.headConfigBootstrap.initializationParameters,
          moved
        )
        mutated.headParamsHash == hc.headParamsHash
    }
}

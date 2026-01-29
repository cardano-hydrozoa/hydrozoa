package hydrozoa.rulebased.ledger.dapp

import cats.*
import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.config.*
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.rulebased.DisputeActor
import hydrozoa.rulebased.DisputeActor.Error.ParseError.Treasury.TreasuryMissing
import hydrozoa.rulebased.ledger.dapp.state.VoteState
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.{L1, Utxo}
import org.scalacheck.{Gen, Properties}
import scala.concurrent.duration.DurationInt
import scalus.builtin.ByteString
import test.Generators.Hydrozoa.{genAdaOnlyPubKeyUtxo, genTreasuryUtxo}
import test.TestM

object DisputeActorTest extends Properties("Joint Ledger Test") {

    import TestM.*

    val _ = property("dispute actor (no actor system)") = run(
      initializer = genHeadConfig,
      testM = for {
          env <- ask[TestHeadConfig]

          ownPeer = env.headConfig.privateNodeSettings.ownPeer

          disputeActorConfig = DisputeActor.Config(
            ownPeerPkh = ownPeer.wallet.exportVerificationKeyBytes,
            tokenNames = env.headConfig.headInstance.tokenNames,
            headMultisigScript = env.headConfig.headParameters.headPeers.headMultisigScript,
            receiveTimeout = 0.seconds,
            cardanoInfo = env.headConfig.cardanoInfo
          )

          // Not sure if this indexing is correct
          collateralUtxo <- pick(
            genAdaOnlyPubKeyUtxo(env.allPeers.toList(ownPeer.peerId.peerNum))
                .label("collateral utxo")
          )

          ////////////////////////////////////////////////////////
          // Scenario: fallback hasn't been submitted yet or was rolled back.
          // Expectation: handleDisputeRes returns unit

          /////////////////////
          // L2 commitment and block header
          /////////////////////
          numUtxos <- pick(Gen.choose[Int](0, 5).label("number of initial l2 utxos"))
          initialL2Utxos <- pick(genUtxosL2(numUtxos).label("initial L2 utxo set"))
          initialCommitment: VoteState.KzgCommitment = {
              import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.*
              ByteString.fromArray(
                IArray.genericWrapArray(calculateCommitment(hashToScalar(initialL2Utxos.asScalus))).toArray
              )
          }

          multisigTreasury <- pick(genTreasuryUtxo(env.allPeers))
          blockHeader = BlockHeader.Minor.Onchain(
            blockNum = 1,
            startTime = BigInt(0),
            versionMajor = multisigTreasury.datum.versionMajor,
            versionMinor = BigInt(2),
            commitment = initialCommitment
          )

          cardanoBackend <- lift(
            CardanoBackendMock.mockIO(
              MockState(initialUtxos = Map.from(List(multisigTreasury.asUtxo.toTuple)))
            )
          )
          disputeActor = DisputeActor(
            config = disputeActorConfig,
            collateralUtxo = Utxo[L1](collateralUtxo),
            blockHeader = blockHeader,
            cardanoBackend = cardanoBackend,
            signatures = signBlockHeader(blockHeader, env.allPeers)
          )

          res <- lift(disputeActor.handleDisputeRes)

      } yield res == Left(TreasuryMissing)
    )
}

package hydrozoa.rulebased.ledger.dapp

import cats.*
import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.*
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.virtual.commitment.{KzgCommitment, TrustedSetup}
import hydrozoa.rulebased.DisputeActor
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.state.{TreasuryState, VoteState}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Properties, Test}
import scala.concurrent.duration.DurationInt
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{BLS12_381_G2_Element, ByteString}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import test.Generators.Hydrozoa.genAdaOnlyPubKeyUtxo
import test.TestM

object DisputeActorTestHelpers {

    import TestM.*

    def mkVoteUtxo(
        key: BigInt,
        link: BigInt,
        voteStatus: VoteStatus
    ): TestM[TestHeadConfig, scalus.cardano.ledger.Utxo] =
        for {
            env <- ask
            ownVoteUtxoInput <- pick(Arbitrary.arbitrary[TransactionInput])
            disputeResAddress = DisputeResolutionScript.address(env.headConfig.headInstance.network)
            ownVoteUtxoOutput = Babbage(
              address = disputeResAddress,
              value = Value.assets(
                lovelace = Coin.ada(5),
                assets = Map(
                  (
                    env.headConfig.headInstance.headMultisigScript.policyId,
                    Map((env.headConfig.headInstance.tokenNames.voteTokenName, 1))
                  )
                )
              ),
              datumOption = Some(
                Inline(toData(VoteState.VoteDatum(key = key, link = link, voteStatus = voteStatus)))
              ),
              scriptRef = None
            )
            ownVoteUtxo = (ownVoteUtxoInput, ownVoteUtxoOutput)
        } yield scalus.cardano.ledger.Utxo(ownVoteUtxo)

    def mkRuleBasedTreasury(
        versionMajor: BigInt,
        coin: Coin
    ): TestM[TestHeadConfig, RuleBasedTreasuryUtxo] =
        for {
            env <- ask[TestHeadConfig]
            treasuryInput <- pick(Arbitrary.arbitrary[TransactionInput])

            datum =
                TreasuryState.UnresolvedDatum(
                  headMp = env.headConfig.headInstance.headMultisigScript.policyId,
                  disputeId = env.headConfig.headInstance.tokenNames.headTokenName.bytes,
                  peers = scalus.prelude.List
                      .from(env.headPeers.map(_.wallet.exportVerificationKeyBytes.bytes).toList),
                  peersN = env.headPeers.size,
                  versionMajor = versionMajor,
                  // FIXME: This shouldn't be empty
                  params = ByteString.empty,
                  // this is cribbed from the CommonGenerators.scala test
                  setup = TrustedSetup
                      .takeSrsG2(10)
                      .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
                )
            treasuryUtxo = RuleBasedTreasuryUtxo(
              utxoId = treasuryInput,
              address = RuleBasedTreasuryScript.address(env.headConfig.headInstance.network),
              datum = RuleBasedTreasuryDatum.Unresolved(datum),
              value = Value.assets(
                assets = Map(
                  (
                    env.headConfig.headInstance.headMultisigScript.policyId,
                    Map((env.headConfig.headInstance.tokenNames.headTokenName, 1))
                  )
                ),
                lovelace = coin
              )
            )

        } yield treasuryUtxo

    /** Given a pre-initialized TestM with a TestHeadConfig environment and an initial L2 UTxO set,
      * create a (pure) DisputeActor.
      * @param versionMajor
      *   the major version to vote for
      * @param versionMinor
      *   the minor version to vote for
      * @param initialL1Utxos
      *   the utxos to initialize the cardanoBackend with
      * @param initialL2Utxos
      *   the utxos to initialize the KZG commitment with
      * @param addCollateralUtxoToBackend
      *   true if we should add a generated a collateral utxo to the backend, false if we should
      *   not.
      */
    def mkDisputeActor(
        versionMajor: BigInt,
        versionMinor: BigInt,
        initialL1Utxos: Utxos,
        initialL2Utxos: UtxoSetL2,
        addCollateralUtxo: Boolean = true
    ): TestM[TestHeadConfig, DisputeActor] =
        for {
            env <- ask[TestHeadConfig]

            ownPeer = env.headConfig.privateNodeSettings.ownPeer

            disputeActorConfig = DisputeActor.Config(
              ownPeerPkh = ownPeer.wallet.exportVerificationKeyBytes.pubKeyHash,
              tokenNames = env.headConfig.headInstance.tokenNames,
              headMultisigScript = env.headConfig.headParameters.headPeers.headMultisigScript,
              receiveTimeout = 0.seconds,
              cardanoInfo = env.headConfig.cardanoInfo
            )

            // Not sure if this indexing is correct
            collateralUtxo <- pick[TestHeadConfig, scalus.cardano.ledger.Utxo](
              genAdaOnlyPubKeyUtxo(env.headPeers.toList(ownPeer.peerId.peerNum))
                  .label("collateral utxo")
            )

            ////////////////////////////////////////////////////////
            // Scenario: fallback hasn't been submitted yet or was rolled back.
            // Expectation: handleDisputeRes returns unit

            /////////////////////
            // L2 commitment and block header
            /////////////////////
            initialCommitment: VoteState.KzgCommitment = {
                import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.*
                ByteString.fromArray(
                  IArray
                      .genericWrapArray(calculateCommitment(hashToScalar(initialL2Utxos.asScalus)))
                      .toArray
                )
            }

            blockHeader = BlockHeader.Minor.Onchain(
              blockNum = 1,
              startTime = BigInt(0),
              versionMajor = versionMajor,
              versionMinor = versionMinor,
              commitment = initialCommitment
            )

            cardanoBackend <- lift(
              CardanoBackendMock.mockIO(
                MockState(initialUtxos =
                    initialL1Utxos ++
                        (if addCollateralUtxo
                         then List((collateralUtxo.input, collateralUtxo.output))
                         else List.empty)
                )
              )
            )
            disputeActor = DisputeActor(
              config = disputeActorConfig,
              collateralUtxo = hydrozoa
                  .Utxo[L1](UtxoId[L1](collateralUtxo.input), Output[L1](collateralUtxo.output)),
              blockHeader = blockHeader,
              cardanoBackend = cardanoBackend,
              signatures = signBlockHeader(blockHeader, env.headPeers)
            )
        } yield disputeActor
}

/*
These tests test the basic functionality of the dispute actor.

The first few tests are sanity checks to ensure that raising exceptions (for unrecoverable failures) and returning
left are handled properly by `handleDisputeRes`.
 */
object DisputeActorTest extends Properties("Dispute Actor Test") {

    import DisputeActorTestHelpers.*
    import TestM.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withInitialSeed(Seed.fromBase64("VQqSeZXfNOrKSH_vYlnW-f7o2OByIhEMhpvpLLvxt0P=").get)
    }

    val _ = property("dispute actor (no actor system)") = run(
      initializer = genHeadConfig,
      testM = for {
          env <- ask[TestHeadConfig]

          // Missing vote datum throws
//      _ <- {
//        for {
//          txHash <- pick(genByteStringOfN(32).map(TransactionHash.fromByteString))
//          index <- pick(Gen.choose(0, 10))
//
//
//          voteInput = TransactionInput(txHash, index)
//          voteOutput = Babbage(
//            address = DisputeResolutionScript.address(env.headConfig.headInstance.network),
//            value = Value.assets(
//              lovelace = Coin.ada(5), assets = Map((
//                env.headConfig.headInstance.headMultisigScript.policyId,
//                Map((env.headConfig.headInstance.tokenNames.voteTokenName, 1)))
//              )),
//            datumOption = None,
//            scriptRef = None
//          )
//          disputeActor <- mkDisputeActor(
//            versionMajor = 100,
//            versionMinor = 2,
//            initialL1Utxos = Map((voteInput, voteOutput)),
//            initialL2Utxos = UtxoSet[L2]()
//          )
//          // Should throw here
//          res <- lift(disputeActor.handleDisputeRes.attempt)
//          _ <- assertWith(
//            msg = "Missing vote datum throws",
//            condition = res == Left(DisputeActor.Error.ParseError.Vote.MissingDatum(hydrozoa.Utxo[L1](UtxoId[L1](voteInput), Output[L1](voteOutput))))
//
//          )
//
//        } yield true
//      }

          // Test: Missing rules-based treasury utxo does not throw
//      _ <- for {
//        env <- ask[TestHeadConfig]
//        fallbackTreasury <- pick(genTreasuryUtxo(env.allPeers))
//        disputeActor <- mkDisputeActor(
//          versionMajor = 100,
//          versionMinor = 2,
//          initialL1Utxos = Map.empty,
//          initialL2Utxos = UtxoSet[L2](Map.empty)
//        )
//        res <- lift(disputeActor.handleDisputeRes)
//        _ <- assertWith(
//          msg = "Missing rules best treasury returns Left",
//          condition =
//            res == Left(DisputeActor.Error.ParseError.Treasury.TreasuryMissing)
//        )
//      } yield true

          // Own uncast vote utxo and other uncast vote utxo exist -- Vote is cast
          _ <- for {
              env <- ask[TestHeadConfig]
              versionMajor = 100
              versionMinor = 2
              initialL2Utxos <- pick(genUtxosL2(10))

              ruleBasedTreasury <- mkRuleBasedTreasury(
                versionMajor,
                Coin(initialL2Utxos.map((_, o) => o.value.coin.value).sum)
              )

              ownPkh =
                  env.headConfig.privateNodeSettings.ownPeer.wallet.exportVerificationKeyBytes.pubKeyHash

              // One vote awaiting a vote with our pkh
              ownVoteUtxo <- mkVoteUtxo(1, 2, VoteStatus.AwaitingVote(ownPkh))

              // One vote awaiting a vote with a different pkg
              otherVoteUtxo <- mkVoteUtxo(
                2,
                3,
                VoteStatus.AwaitingVote(
                  env.headPeers
                      .filter(_.wallet.exportVerificationKeyBytes.pubKeyHash != ownPkh)
                      .head
                      .wallet
                      .exportVerificationKeyBytes
                      .pubKeyHash
                )
              )

              disputeActor <- mkDisputeActor(
                versionMajor = versionMajor,
                versionMinor = versionMinor,
                initialL1Utxos = Map(
                  (ruleBasedTreasury.utxoId, ruleBasedTreasury.output),
                  ownVoteUtxo.toTuple,
                  otherVoteUtxo.toTuple
                ),
                initialL2Utxos = initialL2Utxos
              )
              _ <- lift(disputeActor.handleDisputeRes)
              queryResEither <- lift(
                disputeActor.cardanoBackend.utxosAt(
                  DisputeResolutionScript.address(env.headConfig.headInstance.network)
                )
              )
              Right(queryRes) = queryResEither

              _ <- assertWith(
                msg = "utxo set size stays the same after casting vote",
                condition = queryRes.size == 2
              )

              votedOutput = queryRes.asScalus
                  .filter((input, _) => !(otherVoteUtxo.input == input))
                  .head
              _ <- assertWith(
                msg = "Vote output value doesn't change",
                condition = votedOutput._2.value == ownVoteUtxo.output.value
              )
              _ <- assertWith(
                msg = "Vote output address doesn't change",
                condition = votedOutput._2.address == ownVoteUtxo.output.address
              )
              _ <- assertWith(
                msg = "Vote output has correct datum",
                condition = votedOutput._2.datumOption match {
                    case Some(Inline(d)) =>
                        val votedDatum = fromData[VoteDatum](d)
                        votedDatum.key == 1
                        && votedDatum.link == 2
                        && votedDatum.voteStatus == VoteStatus.Voted(
                          commitment = ByteString.fromArray(
                            KzgCommitment
                                .calculateCommitment(
                                  KzgCommitment.hashToScalar(initialL2Utxos.asScalus)
                                )
                                .toArray
                          ),
                          versionMinor = versionMinor
                        )
                    case None => false
                }
              )

          } yield true

      } yield true
    )
}

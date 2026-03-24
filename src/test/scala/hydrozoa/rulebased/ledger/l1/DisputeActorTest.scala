package hydrozoa.rulebased.ledger.l1

import cats.*
import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.eutxol2.toEvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.DisputeActor
import hydrozoa.rulebased.ledger.l1.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.state.{TreasuryState, VoteState}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyM}
import scalus.builtin.BLS12_381_G2_Element
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{genByteStringOfN, given}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.prelude.List as SList
import test.Generators.Hydrozoa.genPubKeyUtxo
import test.{TestM, TestPeersSpec}

object DisputeActorTestHelpers {
    import MultiNodeConfig.*

    def mkVoteUtxo(
        key: BigInt,
        link: BigInt,
        voteStatus: VoteStatus
    ): MultiNodeConfigTestM[scalus.cardano.ledger.Utxo] =
        for {
            env <- ask
            ownVoteUtxoInput <- pick(Arbitrary.arbitrary[TransactionInput])
            disputeResAddress = DisputeResolutionScript.address(env.headConfig.network)
            ownVoteUtxoOutput = Babbage(
              address = disputeResAddress,
              value = Value.assets(
                lovelace = Coin.ada(5),
                assets = Map(
                  (
                    env.headConfig.headMultisigScript.policyId,
                    Map((env.headConfig.headTokenNames.voteTokenName, 1))
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
    ): MultiNodeConfigTestM[RuleBasedTreasuryUtxo] =
        for {
            env <- ask
            treasuryInput <- pick(Arbitrary.arbitrary[TransactionInput])

            datum =
                TreasuryState.UnresolvedDatum(
                  headMp = env.headConfig.headMultisigScript.policyId,
                  disputeId = env.headConfig.headTokenNames.treasuryTokenName.bytes,
                  peers = SList.from(env.headConfig.headPeerVKeys.toList),
                  peersN = env.headConfig.headPeerVKeys.size,
                  versionMajor = versionMajor,
                  // this is cribbed from the CommonGenerators.scala test
                  setup = TrustedSetup
                      .takeSrsG2(10)
                      .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
                )
            treasuryUtxo = RuleBasedTreasuryUtxo(
              utxoId = treasuryInput,
              address = RuleBasedTreasuryScript.address(env.headConfig.network),
              datum = RuleBasedTreasuryDatum.Unresolved(datum),
              value = Value.assets(
                assets = Map(
                  (
                    env.headConfig.headMultisigScript.policyId,
                    Map((env.headConfig.headTokenNames.treasuryTokenName, 1))
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
      * @param initialEvacuationMap
      *   the evacuation map to initialize the KZG commitment with
      * @param addCollateralUtxoToBackend
      *   true if we should add a generated a collateral utxo to the backend, false if we should
      *   not.
      */
    def mkDisputeActor(
        versionMajor: BigInt,
        versionMinor: BigInt,
        initialL1Utxos: Utxos,
        initialEvacuationMap: EvacuationMap,
        addCollateralUtxo: Boolean = true
    ): MultiNodeConfigTestM[DisputeActor] =
        for {
            env <- ask

            ownPeerConfig = env.nodePrivateConfigs.head

            // Not sure if this indexing is correct
            collateralUtxo <- pick(
              genPubKeyUtxo(
                env.headConfig,
                ownPeerConfig._2.ownHeadWallet.address(env.headConfig.network),
                Arbitrary.arbitrary[Coin].map(Value(_))
              )
                  .label("collateral utxo")
            )

            ////////////////////////////////////////////////////////
            // Scenario: fallback hasn't been submitted yet or was rolled back.
            // Expectation: handleDisputeRes returns unit

            /////////////////////
            // L2 commitment and block header
            /////////////////////
            initialCommitment: VoteState.KzgCommitment = initialEvacuationMap.kzgCommitment

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
            disputeActor = DisputeActor(config = env.nodeConfigs.head._2)(
              collateralUtxo = collateralUtxo,
              blockHeader = blockHeader,
              _cardanoBackend = cardanoBackend,
              signatures = env.multisignHeader(blockHeader).toList
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
    import MultiNodeConfig.*

    val _ = property("dispute actor (no actor system)") = run(
      initializer = PropertyM.pick(MultiNodeConfig.generate(TestPeersSpec.default)()),
      testM = for {
          env <- ask

          // Missing vote datum throws
          _ <- {
              for {
                  txHash <- pick(genByteStringOfN(32).map(TransactionHash.fromByteString))
                  index <- pick(Gen.choose(0, 10))

                  voteInput = TransactionInput(txHash, index)
                  voteOutput = Babbage(
                    address = DisputeResolutionScript.address(env.headConfig.network),
                    value = Value.assets(
                      lovelace = Coin.ada(5),
                      assets = Map(
                        (
                          env.headConfig.headMultisigScript.policyId,
                          Map((env.headConfig.headTokenNames.voteTokenName, 1))
                        )
                      )
                    ),
                    datumOption = None,
                    scriptRef = None
                  )
                  disputeActor <- mkDisputeActor(
                    versionMajor = 100,
                    versionMinor = 2,
                    initialL1Utxos = Map((voteInput, voteOutput)),
                    initialEvacuationMap = EvacuationMap.empty
                  )
                  // Should throw here
                  res <- lift(disputeActor.handleDisputeRes.attempt)
                  _ <- assertWith(
                    msg = "Missing vote datum throws",
                    condition = res == Left(
                      DisputeActor.Error.ParseError.Vote.MissingDatum(Utxo(voteInput, voteOutput))
                    )
                  )
              } yield true
          }

          // Test: Missing rules-based treasury utxo does not throw
          _ <- for {
              disputeActor <- mkDisputeActor(
                versionMajor = 100,
                versionMinor = 2,
                initialL1Utxos = Map.empty,
                initialEvacuationMap = EvacuationMap.empty
              )
              res <- lift(disputeActor.handleDisputeRes)
              _ <- assertWith(
                msg = "Missing rules best treasury returns Left",
                condition = res == Left(DisputeActor.Error.ParseError.Treasury.TreasuryMissing)
              )
          } yield true

          // Own uncast vote utxo and other uncast vote utxo exist -- Vote is cast
          _ <- for {
              initialL2Utxos <- pick(genUtxosL2(env.headConfig, 10))
              versionMajor = 100
              versionMinor = 2

              ruleBasedTreasury <- mkRuleBasedTreasury(
                versionMajor,
                Coin(initialL2Utxos.map((_, o) => o.value.coin.value).sum)
              )

              ownWallet =
                  env.nodePrivateConfigs.head._2.ownHeadWallet

              // One vote awaiting a vote with our pkh
              ownVoteUtxo <- mkVoteUtxo(1, 2, VoteStatus.AwaitingVote(ownWallet.pubKeyHash))

              // One vote awaiting a vote with a different pkg
              otherVoteUtxo <- mkVoteUtxo(
                2,
                3,
                VoteStatus.AwaitingVote(
                  peer = env.nodePrivateConfigs
                      .map(_._2)
                      .filter(_.ownHeadVKey != ownWallet.exportVerificationKey)
                      .head
                      .ownHeadWallet
                      .pubKeyHash
                )
              )

              initEvacMap <- failEither(initialL2Utxos.toEvacuationMap(env.headConfig))

              disputeActor <- mkDisputeActor(
                versionMajor = versionMajor,
                versionMinor = versionMinor,
                initialL1Utxos = Map(
                  (ruleBasedTreasury.utxoId, ruleBasedTreasury.output),
                  ownVoteUtxo.toTuple,
                  otherVoteUtxo.toTuple
                ),
                initialEvacuationMap = initEvacMap
              )
              _ <- lift(disputeActor.handleDisputeRes)
              queryResEither <- lift(
                disputeActor.cardanoBackend.utxosAt(
                  DisputeResolutionScript.address(env.headConfig.network)
                )
              )
              Right(queryRes) = queryResEither

              _ <- assertWith(
                msg = "utxo set size stays the same after casting vote",
                condition = queryRes.size == 2
              )

              votedOutput = queryRes
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

              evacMap <- failEither(initialL2Utxos.toEvacuationMap(env.headConfig))
              _ <- assertWith(
                msg = "Vote output has correct datum",
                condition = votedOutput._2.datumOption match {
                    case Some(Inline(d)) =>
                        val votedDatum = fromData[VoteDatum](d)
                        votedDatum.key == 1
                        && votedDatum.link == 2
                        && votedDatum.voteStatus == VoteStatus.Voted(
                          commitment = evacMap.kzgCommitment,
                          versionMinor = versionMinor
                        )
                    case None => false
                }
              )

          } yield true

      } yield true
    )
}

package hydrozoa.rulebased.ledger.l1

import cats.*
import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{addrKeyHash, pubKeyHash}
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.DisputeActor
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.state.{TreasuryState, VoteState}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.builtin.BLS12_381_G2_Element
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{genByteStringOfN, given}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.onchain.plutus.v3.PosixTime
import test.Generators.Hydrozoa.{genEvacuationMap, genPositiveValue}

// Note: If the vote status is unresolved, the dispute resolution script will fail unless the tx hash matches
// the treasury utxo's tx hash, which is the tx hash of the fallback transaction.
// The generators do some manual threading of this right now, perhaps we move towards more type safety in the future
object DisputeActorTestHelpers {
    import MultiNodeConfig.*

    def mkVoteUtxo(
        key: BigInt,
        link: BigInt,
        voteStatus: VoteStatus,
        // Careful, these can't conflict!
        txIn: TransactionInput,
        nVoteTokens: BigInt = 1,
    ): MultiNodeConfigTestM[scalus.cardano.ledger.Utxo] =
        for {
            env <- ask
            disputeResAddress = HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network)
            ownVoteUtxoOutput = Babbage(
              address = disputeResAddress,
              value = Value.assets(
                lovelace = Coin.ada(5),
                assets = Map(
                  (
                    env.headConfig.headMultisigScript.policyId,
                    Map((env.headConfig.headTokenNames.voteTokenName, nVoteTokens.toLong))
                  )
                )
              ),
              datumOption = Some(
                Inline(toData(VoteState.VoteDatum(key = key, link = link, voteStatus = voteStatus)))
              ),
              scriptRef = None
            )
            ownVoteUtxo = (txIn, ownVoteUtxoOutput)
        } yield scalus.cardano.ledger.Utxo(ownVoteUtxo)

    def mkRuleBasedTreasury(
        versionMajor: BigInt,
        value: Value,
        txIn: TransactionInput,
        votingDeadline: PosixTime
    ): MultiNodeConfigTestM[RuleBasedTreasuryUtxo] =
        for {
            env <- ask

            datum = Unresolved(
              deadlineVoting = votingDeadline,
              versionMajor = versionMajor,
              // this is cribbed from the CommonGenerators.scala test
              setup = TrustedSetup
                  .takeSrsG2(10)
                  .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
            )
            treasuryUtxo = RuleBasedTreasuryUtxo(
              utxoId = txIn,
              treasuryOutput = RuleBasedTreasuryOutput(
                datum,
                value
              )
            )

        } yield treasuryUtxo

    /** Given a pre-initialized TestM with a TestHeadConfig environment and an initial L2 UTxO set,
      * create a (pure) DisputeActor.
      *
      * Automatically adds an ada-only collateral utxo and the reference utxos from the config.
      * @param versionMajor
      *   the major version to vote for
      * @param versionMinor
      *   the minor version to vote for
      * @param additionalL1Utxos
      *   the utxos to initialize the cardanoBackend with
      * @param initialEvacuationMap
      *   the evacuation map to initialize the KZG commitment with
      */
    def mkDisputeActor(
        versionMajor: BigInt,
        versionMinor: BigInt,
        additionalL1Utxos: Utxos,
        initialEvacuationMap: EvacuationMap,
    ): MultiNodeConfigTestM[DisputeActor] =
        for {
            env <- ask

            // If you happen to have reference utxos with the same tx id as other utxos, this will screw things up
            refUtxoIds = env.nodeConfigs.head._2.scriptReferenceUtxos.toList.map(_.input)

            _ <- assertWith(
              refUtxoIds.forall(id => !additionalL1Utxos.contains(id)),
              "Reference utxos from head config conflict with utxos from mkDisputeActor"
            )
            ownPeerConfig = env.nodePrivateConfigs.head

            disputeCollateralUtxo <- pick(
              genCollateralUtxo(
                env.nodePrivateConfigs.head._2.ownHeadWallet.exportVerificationKey.addrKeyHash
              )(using env.headConfig)
                  .label("collateral utxo")
            )

            initialCommitment: VoteState.KzgCommitment = initialEvacuationMap.kzgCommitment

            now <- lift(realTimeQuantizedInstant(env.slotConfig))
            currentSlot = now.toSlot

            blockHeader = BlockHeader.Minor.Onchain(
              blockNum = 1,
              startTime = now.toPosixTime,
              versionMajor = versionMajor,
              versionMinor = versionMinor,
              commitment = initialCommitment
            )

            initialUtxos = additionalL1Utxos ++
                List(
                  (
                    disputeCollateralUtxo.input,
                    disputeCollateralUtxo.collateralOutput
                        .toOutput(using env)
                  )
                )
                ++ env.nodeConfigs.head._2.scriptReferenceUtxos.toList.map(_.toTuple)

            cardanoBackend <- lift(
              CardanoBackendMock.mockIO(
                // FIXME: I think that the current slot in the mock state and the slot in the UtxoEnv of the context
                //  conflict
                MockState(ledgerState = State(initialUtxos), currentSlot = currentSlot),
                mkContext = l =>
                    Context(
                      fee = Coin.zero,
                      env = UtxoEnv.apply(
                        currentSlot.slot,
                        env.headConfig.cardanoProtocolParams,
                        certState = CertState.empty,
                        env.headConfig.network
                      ),
                      slotConfig = env.headConfig.slotConfig,
                      evaluatorMode = EvaluateAndComputeCost
                    )
              )
            )
            disputeActor = DisputeActor(
              blockHeader = blockHeader,
              cardanoBackend = cardanoBackend,
              signatures = env.multisignHeader(blockHeader).toList
            )(using env.nodeConfigs.head._2)
        } yield disputeActor
}

/*
These tests test the basic functionality of the dispute actor.

The first few tests are sanity checks to ensure that raising exceptions (for unrecoverable failures) and returning
left are handled properly by `handleDisputeRes`.
 */
object DisputeActorTest extends Properties("Dispute Actor Test") {
//    override def overrideParameters(p: Test.Parameters): Test.Parameters =
//        p.withInitialSeed(Seed.fromBase64("44OLuP3O46fF64PCbvx2qrfHavqcClJ_Q6KdTz-ZrdC=").get)

    import DisputeActorTestHelpers.*
    import MultiNodeConfig.*

    def missingVoteDatumThrows: MultiNodeConfigTestM[Boolean] = for {
        env <- ask
        txHash <- pick(genByteStringOfN(32).map(TransactionHash.fromByteString))
        index <- pick(Gen.choose(0, 10))

        voteInput = TransactionInput(txHash, index)
        voteOutput = Babbage(
          address = HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network),
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
          additionalL1Utxos = Map((voteInput, voteOutput)),
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

    def missingRuleBasedTreasuryUtxoDoesNotThrow: MultiNodeConfigTestM[Boolean] = for {
        disputeActor <- mkDisputeActor(
          versionMajor = 100,
          versionMinor = 2,
          additionalL1Utxos = Map.empty,
          initialEvacuationMap = EvacuationMap.empty
        )
        res <- lift(disputeActor.handleDisputeRes)
        _ <- assertWith(
          msg = "Missing rules best treasury returns Left",
          condition = res == Left(DisputeActor.Error.ParseError.Treasury.TreasuryMissing)
        )
    } yield true

    // Own uncast vote utxo and other uncast vote utxo exist -- Vote is cast
    def votingHappyPath: MultiNodeConfigTestM[Boolean] = for {
        env <- ask
        treasuryToken =
            Value.asset(
              env.headConfig.headMultisigScript.policyId,
              env.headConfig.headTokenNames.treasuryTokenName,
              1
            )
        fallbackTxId <- pick(Arbitrary.arbitrary[TransactionHash])
        nEvacs <- pick(Gen.choose(0, 1000))
        evacMap <- pick(genEvacuationMap(nEvacs)(using env))
        versionMajor = 100
        versionMinor = 2
        now <- lift(realTimeQuantizedInstant(env.headConfig.slotConfig))

        ruleBasedTreasury <- mkRuleBasedTreasury(
          versionMajor,
          // TODO: add equity in this test
          evacMap.totalValue + treasuryToken,
          TransactionInput(fallbackTxId, 0),
          votingDeadline = now.toPosixTime + 600_000
        )

        ownWallet =
            env.nodePrivateConfigs.head._2.ownHeadWallet

        // One vote awaiting a vote with our pkh
        ownVoteUtxo <- mkVoteUtxo(
          1,
          2,
          VoteStatus.AwaitingVote(ownWallet.exportVerificationKey.pubKeyHash),
          TransactionInput(fallbackTxId, env.headConfig.nHeadPeers + 1)
        )

        // One vote awaiting a vote with a different pkh
        otherVoteUtxo <- mkVoteUtxo(
          2,
          3,
          VoteStatus.AwaitingVote(
            peer = env.nodePrivateConfigs.values
                .filter(_.ownHeadVKey != ownWallet.exportVerificationKey)
                .head
                .ownHeadWallet
                .exportVerificationKey
                .pubKeyHash
          ),
          TransactionInput(fallbackTxId, env.headConfig.nHeadPeers + 2)
        )

        disputeActor <- mkDisputeActor(
          versionMajor = versionMajor,
          versionMinor = versionMinor,
          additionalL1Utxos = Map(
            (
              ruleBasedTreasury.utxoId,
              ruleBasedTreasury.treasuryOutput.toOutput(using env.nodeConfigs.head._2)
            ),
            ownVoteUtxo.toTuple,
            otherVoteUtxo.toTuple
          ),
          initialEvacuationMap = evacMap
        )
        _ <- lift(disputeActor.handleDisputeRes)
        queryRes <- lift(
          disputeActor.cardanoBackend.utxosAt(
            HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network)
          )
        ).flatMap(MultiNodeConfig.failLeft)

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

        _ <- assertWith(
          msg = "Vote output has correct datum",
          condition = votedOutput._2.datumOption match {
              case Some(Inline(d)) =>
                  val votedDatum = fromData[VoteDatum](d)(using VoteState.given_FromData_VoteDatum)
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

    def tallyHappyPath: MultiNodeConfigTestM[Boolean] = for {
        env <- ask
        treasuryToken =
            Value.asset(
              env.headConfig.headMultisigScript.policyId,
              env.headConfig.headTokenNames.treasuryTokenName,
              1
            )
        fallbackTxId <- pick(Arbitrary.arbitrary[TransactionHash])
        nEvacs <- pick(Gen.choose(0, 1000))
        evacMap <- pick(genEvacuationMap(nEvacs)(using env))
        versionMajor = 100
        versionMinor = 2
        now <- lift(realTimeQuantizedInstant(env.slotConfig))

        ruleBasedTreasury <- mkRuleBasedTreasury(
          versionMajor,
          // TODO: add equity in this test
          evacMap.totalValue + treasuryToken,
          TransactionInput(fallbackTxId, 0),
          votingDeadline = now.toPosixTime - 600_000
        )

        ownWallet =
            env.nodePrivateConfigs.head._2.ownHeadWallet

        // NOTE: This can conflict with the other txids and cause strange failures. We should probably
        // Keep a running "ResolvedUtxos" in the TestM state to avoid this.
        continuingVoteTxId <- pick(Arbitrary.arbitrary[TransactionHash])
        // One vote awaiting a vote with our pkh
        continuingVoteUtxo <- mkVoteUtxo(
          0,
          1,
          VoteStatus.Voted(evacMap.kzgCommitment, 2),
          TransactionInput(continuingVoteTxId, 0)
        )

        // One vote awaiting a vote with a different pkh
        otherVoteUtxo <- mkVoteUtxo(
          1,
          0,
          VoteStatus.AwaitingVote(
            peer = env.nodePrivateConfigs.values
                .filter(_.ownHeadVKey != ownWallet.exportVerificationKey)
                .head
                .ownHeadWallet
                .exportVerificationKey
                .pubKeyHash
          ),
          TransactionInput(fallbackTxId, env.headConfig.nHeadPeers + 2)
        )

        disputeActor <- mkDisputeActor(
          versionMajor = versionMajor,
          versionMinor = versionMinor,
          additionalL1Utxos = Map(
            (
              ruleBasedTreasury.utxoId,
              ruleBasedTreasury.treasuryOutput.toOutput(using env.nodeConfigs.head._2)
            ),
            continuingVoteUtxo.toTuple,
            otherVoteUtxo.toTuple
          ),
          initialEvacuationMap = evacMap
        )
        _ <- lift(disputeActor.handleDisputeRes)
        queryRes <- lift(
          disputeActor.cardanoBackend.utxosAt(
            HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network)
          )
        ).flatMap(MultiNodeConfig.failLeft)

    } yield true

    def resolutionHappyPath: MultiNodeConfigTestM[Boolean] = for {
        env <- ask
        treasuryToken =
            Value.asset(
              env.headConfig.headMultisigScript.policyId,
              env.headConfig.headTokenNames.treasuryTokenName,
              1
            )

        nEvacs <- pick(Gen.choose(0, 1000))
        evacMap <- pick(genEvacuationMap(nEvacs)(using env))

        voteTxId <- pick(Arbitrary.arbitrary[TransactionHash])
        finalVoteUtxo <- mkVoteUtxo(
          0,
          1,
          voteStatus = Voted(evacMap.kzgCommitment, 1),
          nVoteTokens = BigInt(env.headConfig.nHeadPeers.convert + 1),
          txIn = TransactionInput(voteTxId, 88)
        )

        fallbackTxId <- pick(Arbitrary.arbitrary[TransactionHash].suchThat(_ != voteTxId))
        treasuryEquity <- pick(genPositiveValue)
        now <- lift(realTimeQuantizedInstant(env.slotConfig))
        rulesBasedTreasury <- mkRuleBasedTreasury(
          100,
          evacMap.totalValue + treasuryToken + treasuryEquity,
          TransactionInput(fallbackTxId, 77),
          now.toPosixTime
        )

        da <- mkDisputeActor(
          100,
          1,
          Map(finalVoteUtxo.toTuple, rulesBasedTreasury.toUtxo(using env).toTuple),
          evacMap
        )

        _ <- lift(da.handleDisputeRes)
        utxosAtResolutionAddress <- lift(
          da.cardanoBackend.utxosAt(HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network))
        )
            .flatMap(failLeft)
        utxosAtTreasuryAddress <- lift(
          da.cardanoBackend.utxosAt(HydrozoaBlueprint.mkTreasuryAddress(env.headConfig.network))
        ).flatMap(failLeft)

        _ <- assertWith(
          utxosAtResolutionAddress.isEmpty,
          "There should be no utxos at the resolution address" +
              s"after dispute resolution, but we found ${utxosAtResolutionAddress}"
        )

        _ <- assertWith(
          utxosAtTreasuryAddress.size == 1,
          "There should be 1 utxo at the treasury address" +
              s"after dispute resolution, but we found ${utxosAtTreasuryAddress}"
        )

    } yield true

    val _ = property("dispute actor (no actor system)") = runDefault(
      for {
          _ <- missingVoteDatumThrows
          _ <- missingRuleBasedTreasuryUtxoDoesNotThrow
          _ <- votingHappyPath
          _ <- tallyHappyPath
          _ <- resolutionHappyPath
      } yield true
    )
}

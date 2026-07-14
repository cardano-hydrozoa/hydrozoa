package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{Abstain, Voted}
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.{BallotBox, BallotBoxOutput, RuleBasedRegimeUtxo}
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.ByteString

/** Public (Open-phase) ballot box datum: `Voted` with a prev versionMinor strictly below
  * `targetVersionMinor`, or `Abstain`. Ratchet target selection converges deterministically on the
  * lowest-versionMinor Open box below the target.
  */
def genOpenPhaseVoteDatum(targetVersionMinor: BigInt): Gen[VoteDatum] = {
    val genVoted: Gen[VoteDatum] = for {
        prevMinor <- Gen.choose(0L, (targetVersionMinor - 1).toLong.max(0L)).map(BigInt(_))
        commitment <- genByteStringOfN(48)
    } yield VoteDatum(
      key = 0,
      link = 1,
      voteStatus = Voted(commitment, prevMinor)
    )
    val genAbstain: Gen[VoteDatum] =
        Gen.const(VoteDatum(key = 0, link = 1, voteStatus = Abstain))
    Gen.oneOf(genVoted, genAbstain)
}

def genOpenBallotBox(
    fallbackTxId: TransactionHash,
    voteDatum: VoteDatum
)(using
    HeadPeers.Section & HasTokenNames & CardanoNetwork.Section
): Gen[BallotBox[Voted | Abstain.type]] =
    for {
        outputIx <- Gen.choose(1, 8)
        input = TransactionInput(fallbackTxId, outputIx)
        ballotBoxOutput = BallotBoxOutput(
          key = voteDatum.key,
          link = voteDatum.link,
          coin = Coin.ada(10),
          voteTokens = PositiveInt.unsafeApply(1),
          status = voteDatum.voteStatus
        )
    } yield BallotBox(
      input = input,
      ballotBoxOutput = ballotBoxOutput.asInstanceOf[BallotBoxOutput[Voted | Abstain.type]]
    )

def genRatchetVoteTxBuilder(using multiNodeConfig: MultiNodeConfig): Gen[RatchetVoteTx.Build] = {
    given config: RatchetVoteTx.Config = multiNodeConfig.nodeConfigs.head._2

    // Guarantee versionMinor >= 1 so `Voted(_, prevMinor)` with `prevMinor < versionMinor` and
    // `Abstain` (implicit prevMinor = 0) are both ratchet-able.
    for {
        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        treasuryDatum <- genTreasuryUnresolvedDatum(versionMajor)(using multiNodeConfig)
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)

        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          fallbackTxId = fallbackTxId,
          treasuryDatum,
          ArbitraryInstances.given_Arbitrary_Value.arbitrary
        )

        // The regime utxo (HRWT beacon + head-identity datum) referenced by the ratchet tx
        regimeTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        regimeUtxo = RuleBasedRegimeUtxo(TransactionInput(regimeTxId, 0))

        blockHeader <- genOnchainBlockHeader(versionMajor).suchThat(_.versionMinor > 0)
        voteDatum <- genOpenPhaseVoteDatum(blockHeader.versionMinor)
        openBox <- genOpenBallotBox(fallbackTxId = fallbackTxId, voteDatum = voteDatum)

        signatures = multiNodeConfig.multisignHeader(blockHeader)
        coilSignatures = multiNodeConfig.multisignHeaderCoil(blockHeader)

        // Ratchet path signer = own wallet (Plutus skips the peer-signature check for Open boxes).
        collateralUtxo <- genCollateralUtxo(
          config.ownWallet.exportVerificationKey.addrKeyHash
        )
    } yield RatchetVoteTx.Build(
      openBallotBox = openBox,
      treasuryUtxo = treasuryUtxo,
      regimeUtxo = regimeUtxo,
      collateralUtxo = collateralUtxo,
      sec = blockHeader,
      signatures = signatures.toList,
      coilSignatures = coilSignatures
    )
}

/** Build a builder whose SEC's versionMinor is <= the open box's prev versionMinor — expected to
  * produce `RatchetVoteTx.Build.Error.NotMonotonic`.
  */
def genStaleRatchetBuilder(using
    multiNodeConfig: MultiNodeConfig
): Gen[RatchetVoteTx.Build] = {
    given config: RatchetVoteTx.Config = multiNodeConfig.nodeConfigs.head._2

    for {
        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        treasuryDatum <- genTreasuryUnresolvedDatum(versionMajor)(using multiNodeConfig)
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)

        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          fallbackTxId = fallbackTxId,
          treasuryDatum,
          ArbitraryInstances.given_Arbitrary_Value.arbitrary
        )

        // The regime utxo (HRWT beacon + head-identity datum) referenced by the ratchet tx
        regimeTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        regimeUtxo = RuleBasedRegimeUtxo(TransactionInput(regimeTxId, 0))

        prevMinor <- Gen.choose(1L, 100L).map(BigInt(_))
        secMinor <- Gen.choose(0L, prevMinor.toLong).map(BigInt(_)) // secMinor <= prevMinor
        commitment <- genByteStringOfN(48)
        secCommitment <- genByteStringOfN(48)
        secHeader = hydrozoa.rulebased.ledger.l1.state
            .StandaloneEvacuationCommitmentOnchain(
              headId = config.headTokenNames.treasuryTokenName.bytes,
              versionMajor = versionMajor,
              versionMinor = secMinor,
              commitment = secCommitment
            )
        voteDatum = VoteDatum(
          key = 0,
          link = 1,
          voteStatus = Voted(commitment, prevMinor)
        )
        openBox <- genOpenBallotBox(fallbackTxId = fallbackTxId, voteDatum = voteDatum)

        signatures = multiNodeConfig.multisignHeader(secHeader)
        coilSignatures = multiNodeConfig.multisignHeaderCoil(secHeader)

        collateralUtxo <- genCollateralUtxo(
          config.ownWallet.exportVerificationKey.addrKeyHash
        )
    } yield RatchetVoteTx.Build(
      openBallotBox = openBox,
      treasuryUtxo = treasuryUtxo,
      regimeUtxo = regimeUtxo,
      collateralUtxo = collateralUtxo,
      sec = secHeader,
      signatures = signatures.toList,
      coilSignatures = coilSignatures
    )
}

object RatchetVoteTxTest extends Properties("Ratchet Vote Tx Test") {
    import MultiNodeConfig.*

    val _ = property("builds a well-formed ratchet tx") = runWithCoil()(
      for {
          mnc <- ask
          _ <- {
              given MultiNodeConfig = mnc
              given RatchetVoteTx.Config = mnc.nodeConfigs.head._2
              for {
                  builder <- pick(genRatchetVoteTxBuilder)
                  tx <- failLeft(builder.result)
                  _ <- assertWith(
                    tx.ballotBoxSpent == builder.openBallotBox,
                    "Spent open box should match builder input"
                  )
                  producedStatus = tx.ballotBoxProduced.ballotBoxOutput.status
                  _ <- assertWith(
                    producedStatus.commitment == builder.sec.commitment &&
                        producedStatus.versionMinor == builder.sec.versionMinor,
                    "Produced box status must equal (sec.commitment, sec.versionMinor)"
                  )
                  _ <- assertWith(tx.tx != null, "Transaction should not be null")
              } yield ()
          }
      } yield true
    )

    val _ = property("rejects a stale SEC (versionMinor not strictly greater)") = runWithCoil()(
      for {
          mnc <- ask
          _ <- {
              given MultiNodeConfig = mnc
              given RatchetVoteTx.Config = mnc.nodeConfigs.head._2
              for {
                  builder <- pick(genStaleRatchetBuilder)
                  _ <- assertWith(
                    builder.result match {
                        case Left(RatchetVoteTx.Build.Error.NotMonotonic(_)) => true
                        case _                                               => false
                    },
                    "Stale SEC must be rejected with NotMonotonic"
                  )
              } yield ()
          }
      } yield true
    )
}

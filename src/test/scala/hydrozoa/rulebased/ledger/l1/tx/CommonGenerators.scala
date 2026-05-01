package hydrozoa.rulebased.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.ledger.{CollateralOutput, CollateralUtxo}
import hydrozoa.lib.cardano.scalus.gens.Base
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import monocle.{Focus, Lens}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Value, BlockHeader as _, *}
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v3.TokenName
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.bls12_381.G2Element
import test.*
import registry.*
import registry.scalacheck.*
import CommonGeneratorsTypes.*

/** Common test generators for rule-based transaction tests.
  */
object CommonGenerators {

    lazy val gens =
            gen[EvacuationTx] +:
            gen[CollateralUtxo] +:
            gen(genOnchainBlockHeader) +:
            gen(genTreasuryUnresolvedDatum) +:
            gen[RuleBasedTreasuryUtxo] +:
            gen[RuleBasedTreasuryOutput] +:
            gen[RuleBasedTreasuryDatum] +:
            mapOfN[TransactionInput, TransactionOutput](2) +:
            gen[CollateralOutput] +:
            gen(Focus[EvacuationTx](_.tx)) +:
            gen(ResolvedUtxos.empty) +:
            gen(genDeadlineVoting) +:
            gen(unresolvedSetup) +:
            gen(genVersion) +:
            gen(genVersionMajor) +:
            gen(genVersionMinor) +:
            gen(genPositiveInt) *:
            Base.gens

    // TODO: remove, looks redundant
    def genHeadParams: Gen[
      (
          HeadMultisigScript,
          TokenName,
          NonEmptyList[TestPeerName], // TODO: what's that?
          NonEmptyList[VerificationKey],
          ByteString,
          BigInt,
          TransactionHash
      )
    ] =
        for {
            // This is 4 bytes shorter to accommodate CIP-67 prefixes
            // NB: we use the same token name _suffix_ for all head tokens so far, which is not the case in reality
            headTokenSuffix <- genByteStringOfN(28)
            multiNodeConfig <- MultiNodeConfig.generate(TestPeersSpec.default)()
            params <- genByteStringOfN(32)
            versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
            fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        } yield (
          multiNodeConfig.headConfig.headMultisigScript,
          headTokenSuffix,
          NonEmptyList.fromListUnsafe(
            List.range(0, multiNodeConfig.nodeConfigs.size).map(TestPeerName.fromOrdinal)
          ),
          multiNodeConfig.headConfig.headPeerVKeys,
          params,
          versionMajor,
          fallbackTxId
        )

    def genRuleBasedTreasuryUtxo(
        section: CardanoNetwork.Section & HasTokenNames & HeadPeers.Section,
        fallbackTxId: TransactionHash,
        unresolvedDatum: Unresolved
    ): Gen[RuleBasedTreasuryUtxo] =
        for {
            adaAmount <- Arbitrary
                .arbitrary[Coin]
                .map(c => Coin(math.abs(c.value) + 1000000L)) // Ensure minimum ADA

            // Treasury is always the first output of the fallback tx
            txId = TransactionInput(fallbackTxId, 0)
            scriptAddr = HydrozoaBlueprint.mkTreasuryAddress(section.network)

            beaconTokenAssetName = AssetName(section.headTokenNames.treasuryTokenName.bytes)
            beaconToken = Value.asset(section.headMultisigScript.policyId, beaconTokenAssetName, 1)
            output = RuleBasedTreasuryOutput(
              unresolvedDatum,
              Value(adaAmount) + beaconToken
            )
        } yield RuleBasedTreasuryUtxo(
          utxoId = txId,
          treasuryOutput = output
        )

    def genCollateralUtxo(addrKeyHash: AddrKeyHash): Gen[CollateralUtxo] =
        for {
            input <- arbitrary[TransactionInput]
            coin <- arbitrary[Coin].map(_ + Coin.ada(100))
        } yield CollateralUtxo(
          input,
          CollateralOutput(addrKeyHash, ShelleyDelegationPart.Null, coin, None, None)
        )

    def genOnchainBlockHeader(versionMajor: VersionMajor): Gen[BlockHeader.Minor.Onchain] =
        for {
            blockNum <- Gen.choose(10L, 20L).map(BigInt(_))
            timeCreation <- Gen.choose(1591566491L, 1760000000L).map(BigInt(_))
            versionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
            commitment <- genByteStringOfN(48) // KZG commitment (G1 compressed point)
        } yield BlockHeader.Minor.Onchain(
          blockNum = blockNum,
          startTime = timeCreation,
          versionMajor = versionMajor,
          versionMinor = versionMinor,
          commitment = commitment
        )


    def genPositiveInt(genInt: Gen[Int]): Gen[PositiveInt] =
        genInt.flatMap(i => PositiveInt.apply(i).map(Gen.const).getOrElse(genPositiveInt(genInt)))
}

object CommonGeneratorsTypes:
    opaque type Version <: (BigInt, BigInt) = (BigInt, BigInt)
    opaque type VersionMajor <: BigInt = BigInt
    opaque type VersionMinor <: BigInt = BigInt
    opaque type DeadlineVoting = BigInt
    opaque type UnresolvedSetup = prelude.List[ByteString]

    def genVersion(major: VersionMajor, minor: VersionMinor): Version =
        (major, minor)

    def genVersionMajor: Gen[VersionMajor] =
        Gen.choose(1L, 10L).map(BigInt(_))

    def genVersionMinor: Gen[VersionMinor] =
        Gen.choose(1L, 99L).map(BigInt(_))


    def unresolvedSetup: UnresolvedSetup = {
        TrustedSetup
            .takeSrsG2(10)
            .map(p2 => G2Element(p2).toCompressedByteString)
    }

    def genDeadlineVoting: Gen[DeadlineVoting] =
        Gen
            .choose(600_000, 1800_000)
            .map(BigInt(_))
            .map(System.currentTimeMillis() + _.abs)

    def genTreasuryUnresolvedDatum(versionMajor: VersionMajor, deadlineVoting: DeadlineVoting, setup: UnresolvedSetup): Gen[Unresolved] =
        Unresolved(
            deadlineVoting,
            versionMajor,
            setup
        )

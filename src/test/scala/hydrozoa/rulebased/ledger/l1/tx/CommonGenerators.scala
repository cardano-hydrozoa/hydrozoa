package hydrozoa.rulebased.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.ledger.eutxol2.toEvacuationMap
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.ledger.{CollateralOutput, CollateralUtxo}
import hydrozoa.lib.cardano.scalus.gens.Base
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.{Resolved, Unresolved}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import monocle.{Focus, Lens}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.address.ShelleyPaymentPart.Key
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
import hydrozoa.rulebased.ledger.l1.state.VoteState
import scalus.uplc.builtin.ByteString.hex

/** Common test generators for rule-based transaction tests.
  */
object CommonGenerators {

    lazy val gens =
        gen(genEvacuationMap) +:
            gen((_: HeadConfig).headPeers) +:
            gen((_: NodeConfig).headConfig) +:
            gen((_: MultiNodeConfig).nodeConfigs.values.head) +:
            share[MultiNodeConfig] +:
            gen(MultiNodeConfig.generateDefault) +:
            gen[EvacuationTx] +:
            gen(genOnchainBlockHeader) +:
            gen(genCollateralUtxo) +:
            gen(membershipProofFromKzg) +:
            gen(resolvedSetup) +:
            gen(genKzgCommitment) +:
            gen(genSetupSize) +:
            gen[RuleBasedTreasuryUtxo] +:
            gen[RuleBasedTreasuryOutput] +:
            gen[RuleBasedTreasuryDatum] +:
            gen((utxo: Utxo) => Map(utxo.toTuple)) +:
            gen[CollateralOutput] +:
            gen(Focus[EvacuationTx](_.tx)) +:
            gen(ResolvedUtxos.empty) +:
            listOfMinMax[ScalusScalar](64, 1024) +:
            gen(genScalusScalar) +:
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
            // Bounded to avoid Long-overflow when summed with other Coin values downstream.
            coin <- Gen.choose(0L, 100_000_000L).map(c => Coin(c) + Coin.ada(100))
        } yield CollateralUtxo(
          input,
          CollateralOutput(addrKeyHash, ShelleyDelegationPart.Null, coin, None, None)
        )

    def genEvacuationMap(utxos: Utxos, headConfig: HeadConfig) =
        val Right(evacMap) = utxos.toEvacuationMap(headConfig)
        evacMap

    def genOnchainBlockHeader(versionMajor: VersionMajor, versionMinor: VersionMinor, commitment: VoteState.KzgCommitment): Gen[BlockHeader.Minor.Onchain] =
        for {
            blockNum <- Gen.choose(10L, 20L).map(BigInt(_))
            timeCreation <- Gen.choose(1591566491L, 1760000000L).map(BigInt(_))
        } yield BlockHeader.Minor.Onchain(
          blockNum = blockNum,
          startTime = timeCreation,
          versionMajor,
          versionMinor,
          commitment,
        )

    def genPositiveInt(genInt: Gen[Int]): Gen[PositiveInt] =
        genInt.flatMap(i => PositiveInt.apply(i).map(Gen.const).getOrElse(genPositiveInt(genInt)))

    def genScalarList: Gen[prelude.List[ScalusScalar]] =
        for {
            length <- Gen.choose(64, 1024)
            list <- Gen.listOfN(length, genScalusScalar)
        } yield prelude.List.from(list)

    /** Generate a big enough (> 2^230) scalus.Scalar
      */
    def genScalusScalar: Gen[ScalusScalar] =
        for {
            bigInt <- Gen.choose(
              BigInt("1000000000000000000000000000000000000000000000000000000000000000000000"),
              ScalusScalar.fieldPrime - 1
            )
        } yield ScalusScalar.applyUnsafe(bigInt)

    def someShelleyAddress(config: CardanoNetwork.Section, keyHash: AddrKeyHash): ShelleyAddress =
        ShelleyAddress(
          network = config.network,
          payment = Key(keyHash),
          delegation = ShelleyDelegationPart.Null
        )

}

object CommonGeneratorsTypes:
    opaque type Version <: (BigInt, BigInt) = (BigInt, BigInt)
    opaque type VersionMajor <: BigInt = BigInt
    opaque type VersionMinor <: BigInt = BigInt
    opaque type DeadlineVoting = BigInt
    opaque type UnresolvedSetup = prelude.List[ByteString]
    // Registry-side opaque aliases — give the registry distinct type tags for these values so
    // generators can be wired up without colliding with the ambient Gen[ByteString] / Gen[Int] /
    // Gen[List[ByteString]] channels. Note: no `<: ByteString` (etc.) bound — that bound makes
    // Gen[KzgCommitment] <:< Gen[ByteString], so any entry producing Gen[KzgCommitment] would
    // also satisfy Gen[ByteString] requests anywhere in the registry, accidentally overriding
    // arb[ByteString] and pulling Gen[Utxos] into ScriptRef-style paths (cycle).
    opaque type KzgCommitment = ByteString
    opaque type MembershipProof = ByteString
    opaque type SetupSize = Int
    opaque type ResolvedSetup = prelude.List[ByteString]
    // L1 fee utxos paying the transaction fee — distinct from the L2 `Utxos` set being evacuated,
    // so the registry can resolve `feeUtxos: FeeUtxos` and `utxos: Utxos` to different generators
    // even though both are structurally `Map[TransactionInput, TransactionOutput]`.
    opaque type FeeUtxos = Utxos

    // Coercions from raw underlying types — only available outside the object via these helpers,
    // matching the `genVersion(major, minor)` pattern.
    def kzgCommitment(b: ByteString): KzgCommitment = b
    def membershipProof(b: ByteString): MembershipProof = b
    def setupSize(n: Int): SetupSize = n
    def feeUtxos(u: Utxos): FeeUtxos = u

    extension (fu: FeeUtxos) def toUtxos: Utxos = fu

    /** L1 fee utxo set: a single 100-ADA pub-key utxo at the payment address. Generous enough to
      * cover any plausible transaction fee in these tests; consumers should refine this if a
      * tighter value is needed.
      */
    def genFeeUtxos(txInput: TransactionInput, paymentAddr: ShelleyAddress): FeeUtxos =
        feeUtxos(Map(txInput -> Babbage(paymentAddr, Value.ada(100))))

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

    /** Default placeholder commitment — a known-valid 48-byte compressed G1 point. Tests that
      * actually drive the on-chain validator should override `Gen[KzgCommitment]` with one derived
      * from the real evacuation map.
      */
    def genKzgCommitment: Gen[KzgCommitment] =
        Gen.const(
          hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
        )

    /** A `KzgCommitment` is structurally also the membership-proof byte string used in
      * `Resolved.evacuationActive`. This wires `Gen[KzgCommitment] -> Gen[MembershipProof]`.
      */
    def membershipProofFromKzg(commitment: KzgCommitment): MembershipProof = commitment

    /** Default trusted-setup size. Tests with larger evacuation sets should refine this for the
      * `Resolved` path: `refineGen[RuleBasedTreasuryDatum.Resolved](setupSize(N))`.
      */
    def genSetupSize: Gen[SetupSize] = Gen.const(10)

    def resolvedSetup(size: SetupSize): ResolvedSetup =
        TrustedSetup
            .takeSrsG2(size)
            .map(p2 => G2Element(p2).toCompressedByteString)

    def genDeadlineVoting: Gen[DeadlineVoting] =
        Gen
            .choose(600_000, 1800_000)
            .map(BigInt(_))
            .map(System.currentTimeMillis() + _.abs)

    def genTreasuryUnresolvedDatum(
        versionMajor: VersionMajor,
        deadlineVoting: DeadlineVoting,
        setup: UnresolvedSetup
    ): Gen[Unresolved] =
        Unresolved(
          deadlineVoting,
          versionMajor,
          setup
        )

    def genTreasuryResolvedDatum(
        version: Version,
        evacuationActive: MembershipProof,
        setup: ResolvedSetup
    ): Resolved =
        Resolved(
          evacuationActive = evacuationActive,
          version = version,
          setup = setup
        )

    def genEmptyTreasuryResolvedDatum(version: Version): Gen[Resolved] =
        Resolved(
          evacuationActive =
              hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
          version,
          setup = prelude.List.empty
        )

package hydrozoa.lib.cardano.scalus.gens

import _root_.scalus.cardano.address.*
import _root_.scalus.cardano.ledger.*
import _root_.scalus.cardano.ledger.ArbitraryInstances.given
import _root_.scalus.cardano.ledger.ArbitraryInstances.genByteStringOfN
import _root_.scalus.uplc.builtin.{ByteString, Data}
import _root_.scalus.cardano.onchain.plutus.v1.PubKeyHash
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import registry.value
import registry.scalacheck.*

import scala.collection.immutable.SortedMap

import hydrozoa.lib.cardano.scalus.gens.Containers.{
    keepRaw,
    preludeListOf,
    sized,
    sortedMapOf,
    taggedOrderedSetOf,
    taggedOrderedStrictSetOf,
    taggedSortedMapOf,
    taggedSortedSetOf,
    taggedSortedStrictMapOf
}
// `KeyOf[K, A]` typeclass instances used by the Tagged*Map combinators
// (e.g. KeyOf[ScriptHash, Script.Native]) live in the TransactionWitnessSet companion.
import _root_.scalus.cardano.ledger.TransactionWitnessSet.given

/** Base generators.
  *
  * Constrained `genX: Gen[X]` are defined ahead of `registry` so the registry sees
  * fully-initialized values. Every type whose constructor enforces an invariant via `require(...)`
  * has a hand-written generator below; types without such invariants (Coin, Word64, ExUnits,
  * Constitution) use `gen`.
  */
object Base:

    lazy val gens =
        gen[BlockFile] *:
            gen[Block] *:
            mapOf[Int, KeepRaw[AuxiliaryData]] *:
            indexedSeqOf[KeepRaw[TransactionBody]] *:
            indexedSeqOf[KeepRaw[TransactionWitnessSet]] *:
            indexedSeqOf[Int] *:
            gen[BlockHeader] *:
            gen(genBlockHeaderBody) *:
            gen[Transaction] *:
            optionOf[KeepRaw[AuxiliaryData]] *:
            keepRaw[TransactionBody] *:
            keepRaw[TransactionWitnessSet] *:
            keepRaw[AuxiliaryData] *:
            gen[TransactionBody] *:
            gen[TransactionWitnessSet] *:
            taggedSortedSetOf[VKeyWitness] *:
            taggedSortedSetOf[BootstrapWitness] *:
            taggedSortedMapOf[ScriptHash, Script.Native] *:
            taggedSortedStrictMapOf[ScriptHash, Script.PlutusV1] *:
            taggedSortedStrictMapOf[ScriptHash, Script.PlutusV2] *:
            taggedSortedStrictMapOf[ScriptHash, Script.PlutusV3] *:
            keepRaw[TaggedSortedMap[DataHash, KeepRaw[Data]]] *:
            taggedSortedMapOf[DataHash, KeepRaw[Data]] *:
            keepRaw[Data] *:
            optionOf[KeepRaw[Redeemers]] *:
            keepRaw[Redeemers] *:
            gen[Redeemers] *:
            gen[AuxiliaryData] *:
            optionOf[Map[Word64, Metadatum]] *:
            mapOf[Word64, Metadatum] *:
            indexedSeqOf[Timelock] *:
            iArrayOf[Byte] *:
            indexedSeqOf[ByteString] *:
            listOf[ByteString] *:
            preludeListOf[ByteString] *:
            taggedSortedSetOf[TransactionInput] *:
            gen[TransactionInput] *:
            indexedSeqOf[Sized[TransactionOutput]] *:
            optionOf[Sized[TransactionOutput]] *:
            listOf[TransactionOutput] *:
            sized[TransactionOutput] *:
            gen[TransactionOutput] *:
            gen(genVKeyWitness) *:
            gen(genBootstrapWitness) *:
            gen(genOperationalCert) *:
            gen(genVrfCert) *:
            optionOf[VotingProcedures] *:
            gen[VotingProcedures] *:
            sortedMapOf[Voter, SortedMap[GovActionId, VotingProcedure]] *:
            sortedMapOf[GovActionId, VotingProcedure] *:
            gen[VotingProcedure] *:
            taggedOrderedSetOf[ProposalProcedure] *:
            gen[ProposalProcedure] *:
            gen[GovAction] *:
            gen[Constitution] *:
            taggedOrderedStrictSetOf[Certificate] *:
            gen[Certificate] *:
            gen[VotingProcedure] *:
            gen[Voter] *:
            optionOf[Withdrawals] *:
            gen[Withdrawals] *:
            sortedMapOf[RewardAccount, Coin] *:
            gen[ProtocolParamUpdate] *:
            optionOf[CostModels] *:
            gen[CostModels] *:
            mapOf[Int, IndexedSeq[Long]] *:
            indexedSeqOf[Long] *:
            indexedSeqOf[Relay] *:
            gen[Relay] *:
            optionOf[ByteString] *:
            optionOf[NonNegativeInterval] *:
            optionOf[UnitInterval] *:
            optionOf[ExUnitPrices] *:
            optionOf[ExUnits] *:
            optionOf[PoolVotingThresholds] *:
            optionOf[DRepVotingThresholds] *:
            optionOf[ScriptHash] *:
            optionOf[DataHash] *:
            optionOf[DatumOption] *:
            optionOf[ScriptRef] *:
            optionOf[Coin] *:
            optionOf[Long] *:
            optionOf[Int] *:
            optionOf[Anchor] *:
            optionOf[AuxiliaryDataHash] *:
            optionOf[ScriptDataHash] *:
            optionOf[BlockHash] *:
            optionOf[GovActionId] *:
            optionOf[Mint] *:
            gen[PubKeyHash] *:
            taggedSortedSetOf[AddrKeyHash] *:
            gen(genGovActionId) *:
            optionOf[PoolMetadata] *:
            gen(genPoolMetadata) *:
            mapOf[RewardAccount, Coin] *:
            setOf[AddrKeyHash] *:
            gen[ExUnitPrices] *:
            gen[PoolVotingThresholds] *:
            gen[DRepVotingThresholds] *:
            gen[ScriptRef] *:
            gen[ShelleyPaymentPart] *:
            gen[StakeAddress] *:
            gen[StakePayload] *:
            gen[ShelleyDelegationPart] *:
            gen[Script] *:
            gen[PlutusScript] *:
            genTimelock *:
            gen[DatumOption] *:
            gen[Value] *:
            gen[RewardAccount] *:
            mapOf[Credential, Long] *:
            setOf[Credential] *:
            gen[Credential] *:
            gen[DRep] *:
            gen[Voter] *:
            gen[Vote] *:
            gen[RedeemerTag] *:
            gen[StakeAddress] *:
            gen[StakePayload] *:
            gen[Coin] *:
            gen[Word64] *:
            gen[ExUnits] *:
            gen[Pointer] *:
            gen[Language] *:
            arb[Mint] *:
            arb[Data] *:
            arb[Address] *:
            arb[MultiAsset] *:
            gen(genAnchor) *:
            gen(genAddrKeyHash) *:
            gen(genScriptHash) *:
            gen(genPoolKeyHash) *:
            gen(genStakeKeyHash) *:
            gen(genTransactionHash) *:
            gen(genAuxiliaryDataHash) *:
            gen(genScriptDataHash) *:
            gen(genDataHash) *:
            gen(genMetadataHash) *:
            gen(genVrfKeyHash) *:
            gen(genBlockHash) *:
            gen(genAssetName) *:
            gen(genUnitInterval) *:
            gen(genNonNegativeInterval) *:
            gen(genSlot) *:
            gen(genProtocolVersion) *:
            gen(genNetwork) *:
            arb[String] *:
            arb[Metadatum] *:
            arb[Boolean] *:
            arb[BigInt] *:
            arb[Long] *:
            arb[String] *:
            arb[Int] *:
            arb[ByteString] *:
            arb[Byte]

    val genAddrKeyHash: Gen[AddrKeyHash] = genHash[Blake2b_224, HashPurpose.KeyHash]
    val genScriptHash: Gen[ScriptHash] = genHash[Blake2b_224, HashPurpose.ScriptHash]
    val genPoolKeyHash: Gen[PoolKeyHash] = genHash[Blake2b_224, HashPurpose.PoolKeyHash]
    val genStakeKeyHash: Gen[StakeKeyHash] = genHash[Blake2b_224, HashPurpose.StakeKeyHash]
    val genTransactionHash: Gen[TransactionHash] = genHash[Blake2b_256, HashPurpose.TransactionHash]
    val genAuxiliaryDataHash: Gen[AuxiliaryDataHash] =
        genHash[Blake2b_256, HashPurpose.AuxiliaryDataHash]
    val genScriptDataHash: Gen[ScriptDataHash] = genHash[Blake2b_256, HashPurpose.ScriptDataHash]
    val genDataHash: Gen[DataHash] = genHash[Blake2b_256, HashPurpose.DataHash]
    val genMetadataHash: Gen[MetadataHash] = genHash[Blake2b_256, HashPurpose.MetadataHash]
    val genVrfKeyHash: Gen[VrfKeyHash] = genHash[Blake2b_256, HashPurpose.VrfKeyHash]
    val genBlockHash: Gen[BlockHash] = genHash[Blake2b_256, HashPurpose.BlockHash]

    private def genHash[HF: HashSize, Purpose]: Gen[Hash[HF, Purpose]] =
        genByteStringOfN(summon[HashSize[HF]].size).map(Hash.apply[HF, Purpose])

    /** AssetName: bytes ≤ 32. */
    val genAssetName: Gen[AssetName] =
        Gen.choose(0, 32).flatMap(genByteStringOfN).map(AssetName.apply)

    /** UnitInterval: 0 ≤ numerator ≤ denominator, denominator > 0. */
    val genUnitInterval: Gen[UnitInterval] =
        for
            denominator <- Gen.choose(1L, Long.MaxValue)
            numerator <- Gen.choose(0L, denominator)
        yield UnitInterval(numerator, denominator)

    /** NonNegativeInterval: numerator ≥ 0, denominator > 0. */
    val genNonNegativeInterval: Gen[NonNegativeInterval] =
        for
            numerator <- Gen.choose(0L, Long.MaxValue)
            denominator <- Gen.choose(1L, Long.MaxValue)
        yield NonNegativeInterval(numerator, denominator)

    /** Slot ≥ 0. */
    val genSlot: Gen[Slot] = Gen.choose(0L, Long.MaxValue).map(Slot.apply)

    /** BlockHeaderBody: blockNumber/slot/blockBodySize ≥ 0; issuerVkey/vrfVkey are 32 bytes. */
    val genBlockHeaderBody: Gen[BlockHeaderBody] =
        for
            blockNumber <- Gen.choose(0L, Long.MaxValue)
            slot <- Gen.choose(0L, Long.MaxValue)
            prevHash <- Gen.option(genBlockHash)
            issuerVkey <- genByteStringOfN(32)
            vrfVkey <- genByteStringOfN(32)
            vrfResult <- genVrfCert
            blockBodySize <- Gen.choose(0L, Long.MaxValue)
            blockBodyHash <- genBlockHash
            operationalCert <- genOperationalCert
            protocolVersion <- genProtocolVersion
        yield BlockHeaderBody(
          blockNumber,
          slot,
          prevHash,
          issuerVkey,
          vrfVkey,
          vrfResult,
          blockBodySize,
          blockBodyHash,
          operationalCert,
          protocolVersion
        )

    /** Network: mainnet or testnet */
    val genNetwork: Gen[Network] =
        Gen.oneOf(Seq(Network.Mainnet, Network.Testnet))

    /** ProtocolVersion: major 1–10 (per ctor doc), minor ≥ 0. */
    val genProtocolVersion: Gen[ProtocolVersion] =
        for
            major <- Gen.choose(1, 10)
            minor <- Gen.choose(0, Int.MaxValue)
        yield ProtocolVersion(major, minor)

    /** URL up to 128 ASCII alpha-num chars (matches the constraint shared by Anchor/PoolMetadata).
      */
    private val genUrl: Gen[String] =
        Gen.choose(0, 128).flatMap(n => Gen.listOfN(n, Gen.alphaNumChar)).map(_.mkString)

    /** Anchor: url.length ≤ 128. */
    val genAnchor: Gen[Anchor] =
        for
            url <- genUrl
            dataHash <- genDataHash
        yield Anchor(url, dataHash)

    /** PoolMetadata: url.length ≤ 128. */
    val genPoolMetadata: Gen[PoolMetadata] =
        for
            url <- genUrl
            metadataHash <- genMetadataHash
        yield PoolMetadata(url, metadataHash)

    /** GovActionId: govActionIndex 0..65535 (2-byte index). */
    val genGovActionId: Gen[GovActionId] =
        for
            transactionId <- genTransactionHash
            govActionIndex <- Gen.choose(0, 65535)
        yield GovActionId(transactionId, govActionIndex)

    /** VKeyWitness: vkey 32 bytes, signature 64 bytes. */
    val genVKeyWitness: Gen[VKeyWitness] =
        for
            vkey <- genByteStringOfN(32)
            signature <- genByteStringOfN(64)
        yield VKeyWitness(vkey, signature)

    /** BootstrapWitness: publicKey/chainCode 32 bytes, signature 64 bytes; attributes free. */
    val genBootstrapWitness: Gen[BootstrapWitness] =
        for
            publicKey <- genByteStringOfN(32)
            signature <- genByteStringOfN(64)
            chainCode <- genByteStringOfN(32)
            attributes <- arbitrary[ByteString]
        yield BootstrapWitness(publicKey, signature, chainCode, attributes)

    /** OperationalCert: hotVKey 32 bytes, sigma 64 bytes, sequenceNumber/kesPeriod ≥ 0. */
    val genOperationalCert: Gen[OperationalCert] =
        for
            hotVKey <- genByteStringOfN(32)
            sequenceNumber <- Gen.choose(0L, Long.MaxValue)
            kesPeriod <- Gen.choose(0L, Long.MaxValue)
            sigma <- genByteStringOfN(64)
        yield OperationalCert(hotVKey, sequenceNumber, kesPeriod, sigma)

    /** VrfCert: proof 80 bytes; output free. */
    val genVrfCert: Gen[VrfCert] =
        for
            output <- arbitrary[ByteString]
            proof <- genByteStringOfN(80)
        yield VrfCert(output, proof)

    // Recursive Gen[Timelock]: the `grow` function picks among the recursive variants
    // (AllOf / AnyOf / MOf), each of which embeds an `IndexedSeq[Timelock]` resolved
    // through `self`.
    val genTimelock = genRecursive[Timelock] { self =>
        Gen.oneOf(
          Gen.listOf(self).map(xs => Timelock.AllOf(xs.toIndexedSeq)),
          Gen.listOf(self).map(xs => Timelock.AnyOf(xs.toIndexedSeq)),
          for
              m <- Gen.choose(0, 5)
              xs <- Gen.listOf(self)
          yield Timelock.MOf(m, xs.toIndexedSeq): Timelock
        )
    } *: gen(genTimelockLeaf)

    /** Leaf-only `Timelock` generator — the three non-recursive variants. Used as the base case by
      * the `genRecursive[Timelock]` entry above; the recursive variants (AllOf / AnyOf / MOf) are
      * produced by `genRecursive`'s `grow` function, which threads `self` through.
      */
    def genTimelockLeaf(addrKeyHash: AddrKeyHash, slot: Slot): Gen[Timelock] =
        Gen.oneOf[Timelock](
          Timelock.Signature(addrKeyHash),
          Timelock.TimeStart(slot.slot),
          Timelock.TimeExpire(slot.slot)
        )

package hydrozoa.rulebased.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.UnresolvedDatum
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{BlockHeader as _, Utxo, *}
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v3.TokenName
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{BLS12_381_G2_Element, ByteString}
import test.*
import test.Generators.Hydrozoa.genPubkeyAddress

/** Common test generators for rule-based transaction tests */
object CommonGenerators {

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
            // headPeers = HeadPeers(peers.map(_.wallet.exportVerificationKey))
            // L2 consensus parameters hash
            params <- genByteStringOfN(32)
            // Major version upon switching to the rule-based regime
            versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
            // Fallback tx id - should be common for the vote utxo and treasury utxo
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

    def genTreasuryUnresolvedDatum(
        config: CardanoNetwork.Section & HeadPeers.Section,
        disputeId: TokenName,
        versionMajor: BigInt
    ): Gen[UnresolvedDatum] =
        for {
            deadlineVoting <- Gen
                .choose(600_000, 1800_000)
                .map(BigInt(_))
                .map(System.currentTimeMillis() + _.abs)
            setup = TrustedSetup
                .takeSrsG2(10)
                .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
        } yield UnresolvedDatum(
          headMp = config.headMultisigScript.policyId,
          disputeId = disputeId,
          peers = SList.from(config.headPeerVKeys.iterator),
          peersN = config.nHeadPeers.convert,
          deadlineVoting = deadlineVoting,
          versionMajor = versionMajor,
          setup = setup
        )

    def genRuleBasedTreasuryUtxo(
        config: CardanoNetwork.Section,
        fallbackTxId: TransactionHash,
        headMp: PolicyId,
        treasuryTokenName: TokenName,
        unresolvedDatum: UnresolvedDatum
    ): Gen[RuleBasedTreasuryUtxo] =
        for {
            adaAmount <- Arbitrary
                .arbitrary[Coin]
                .map(c => Coin(math.abs(c.value) + 1000000L)) // Ensure minimum ADA

            // Treasury is always the first output of the fallback tx
            txId = TransactionInput(fallbackTxId, 0)
            spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
            scriptAddr = ShelleyAddress(config.network, spp, ShelleyDelegationPart.Null)

            beaconTokenAssetName = AssetName(treasuryTokenName)
            beaconToken = Value.asset(headMp, beaconTokenAssetName, 1)
        } yield RuleBasedTreasuryUtxo(
          treasuryTokenName = beaconTokenAssetName,
          utxoId = txId,
          address = scriptAddr,
          datum = Unresolved(unresolvedDatum),
          value = Value(adaAmount) + beaconToken
        )

    def genCollateralUtxo(
        config: CardanoNetwork.Section,
        address: ShelleyAddress,
    ): Gen[(TransactionInput, Babbage)] =
        for {
            input <- arbitrary[TransactionInput]
        } yield (
          input,
          Babbage(
            address = address,
            value = Value(Coin(5_000_000L)),
            datumOption = None,
            scriptRef = None
          )
        )

    /** Generate collateral UTXO with random address */
    def genCollateralUtxo(config: CardanoNetwork.Section): Gen[Utxo] =
        for {
            txId <- arbitrary[TransactionHash]
            ix <- Gen.choose(0, 10)
            addr <- genPubkeyAddress(config)
            value <- Gen.choose(5_000_000L, 50_000_000L).map(v => Value(Coin(v)))
        } yield Utxo(
          TransactionInput(txId, ix),
          Babbage(
            address = addr,
            value = value,
            datumOption = None,
            scriptRef = None
          )
        )

    def genOnchainBlockHeader(versionMajor: BigInt): Gen[BlockHeader.Minor.Onchain] =
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

    /** Generator for Shelley address */
    def genShelleyAddress(config: CardanoNetwork.Section): Gen[ShelleyAddress] =
        for {
            keyHash <- arbitrary[AddrKeyHash]
        } yield ShelleyAddress(
          network = config.network,
          payment = ShelleyPaymentPart.Key(keyHash),
          delegation = ShelleyDelegationPart.Null
        )

    /** Generator for L2 UTXO sets */
    def genUtxosL2(config: CardanoNetwork.Section, count: Int = 2): Gen[Utxos] =
        for {
            outputs <- Gen.listOfN(count, genOutputL2(config))
            utxoIds <- Gen.listOfN(count, arbitrary[TransactionInput])
        } yield utxoIds.zip(outputs).toMap

    /** Generator for a single L2 output */
    def genOutputL2(config: CardanoNetwork.Section): Gen[TransactionOutput] =
        for {
            address <- genShelleyAddress(config)
            coin <- Gen.choose(1_000_000L, 10_000_000L)
            value = Value(Coin(coin))
        } yield Babbage(
          address = address,
          value = value,
          datumOption = None,
          scriptRef = None
        )

    /** Generator for version tuple */
    def genVersion: Gen[(BigInt, BigInt)] =
        for {
            major <- Gen.choose(1, 10)
            minor <- Gen.choose(0, 99)
        } yield (BigInt(major), BigInt(minor))

}

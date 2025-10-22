package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.virtual.commitment.TrustedSetup
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.{BlockTypeL2, OnchainBlockHeader, given}
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.UnresolvedDatum
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.Builtins.serialiseData
import scalus.builtin.Data.toData
import scalus.builtin.{BLS12_381_G2_Element, ByteString}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v3.TokenName
import scalus.prelude.List as SList
import scalus.|>
import test.*

// Alias for compatibility
def genPubkeyAddr(network: Network = Mainnet): Gen[ShelleyAddress] = genPubkeyAddress(network)

/** Common test generators for rule-based transaction tests */
object CommonGenerators {

    def genHeadParams: Gen[
      (
          HeadMultisigScript,
          TokenName,
          NonEmptyList[TestPeer],
          NonEmptyList[VerificationKeyBytes],
          ByteString,
          BigInt,
          TransactionHash
      )
    ] =
        for {
            // This is 4 bytes shorter to accommodate CIP-67 prefixes
            // NB: we use the same token name _suffix_ for all head tokens so far, which is not the case in reality
            headTokenSuffix <- genByteStringOfN(28)
            peers <- genTestPeers
            peersVKs = peers.map(_.wallet.exportVerificationKeyBytes)
            hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
            // L2 consensus parameters hash
            params <- genByteStringOfN(32)
            // Major version upon switching to the rule-based regime
            versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
            // Fallback tx id - should be common for the vote utxo and treasury utxo
            fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        } yield (
          hns,
          headTokenSuffix,
          peers,
          peersVKs,
          params,
          versionMajor,
          fallbackTxId
        )

    def genTreasuryUnresolvedDatum(
        headMp: PolicyId,
        disputeId: TokenName,
        peersVKs: NonEmptyList[VerificationKeyBytes],
        params: ByteString,
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
          headMp = headMp,
          disputeId = disputeId,
          peers = SList.from(peersVKs.map(_.bytes).toList),
          peersN = BigInt(peersVKs.length),
          deadlineVoting = deadlineVoting,
          versionMajor = versionMajor,
          params = params,
          setup = setup
        )

    def genRuleBasedTreasuryUtxo(
        fallbackTxId: TransactionHash,
        headMp: PolicyId,
        beaconTokenName: TokenName,
        unresolvedDatum: UnresolvedDatum
    ): Gen[RuleBasedTreasuryUtxo] =
        for {
            adaAmount <- Arbitrary
                .arbitrary[Coin]
                .map(c => Coin(math.abs(c.value) + 1000000L)) // Ensure minimum ADA

            // Treasury is always the first output of the fallback tx
            txId = TransactionInput(fallbackTxId, 0)
            spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
            scriptAddr = ShelleyAddress(Mainnet, spp, ShelleyDelegationPart.Null)

            beaconTokenAssetName = AssetName(beaconTokenName)
            beaconToken = singleton(headMp, beaconTokenAssetName)
        } yield RuleBasedTreasuryUtxo(
          beaconTokenName = beaconTokenAssetName,
          txId = txId,
          addr = scriptAddr,
          datum = Unresolved(unresolvedDatum),
          value = Value(adaAmount) + beaconToken
        )

    def genCollateralUtxo(peer: TestPeer): Gen[(TransactionInput, Babbage)] =
        for {
            input <- arbitrary[TransactionInput]
        } yield (
          input,
          Babbage(
            address = peer.address,
            value = Value(Coin(5_000_000L)),
            datumOption = None,
            scriptRef = None
          )
        )

    /** Generate collateral UTXO with random address */
    def genCollateralUtxo: Gen[Utxo[L1]] =
        for {
            txId <- arbitrary[TransactionHash]
            ix <- Gen.choose(0, 10)
            addr <- genPubkeyAddress(testNetwork)
            value <- Gen.choose(5_000_000L, 50_000_000L).map(v => Value(Coin(v)))
        } yield Utxo[L1](
            UtxoId[L1](TransactionInput(txId, ix)),
            Output[L1](Babbage(
                address = Address[L1](addr),
                value = value,
                datumOption = None,
                scriptRef = None
            ))
        )

    def genOnchainBlockHeader(versionMajor: BigInt): Gen[OnchainBlockHeader] =
        for {
            blockNum <- Gen.choose(10L, 20L).map(BigInt(_))
            timeCreation <- Gen.choose(1591566491L, 1760000000L).map(BigInt(_))
            versionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
            commitment <- genByteStringOfN(48) // KZG commitment (G1 compressed point)
        } yield OnchainBlockHeader(
          blockNum = blockNum,
          blockType = BlockTypeL2.Minor,
          timeCreation = timeCreation,
          versionMajor = versionMajor,
          versionMinor = versionMinor,
          commitment = commitment
        )

    def signBlockHeader(
        blockHeader: OnchainBlockHeader,
        peers: NonEmptyList[TestPeer]
    ): List[Ed25519Signature] = {
        val bs = blockHeader.toData |> serialiseData |> (_.bytes) |> IArray.from
        peers.toList.map(peer => peer.wallet.createEd25519Signature(bs))
    }

    /** Generator for Shelley address */
    def genShelleyAddress: Gen[ShelleyAddress] =
        for {
            keyHash <- arbitrary[AddrKeyHash]
            network = Mainnet
        } yield ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Key(keyHash),
          delegation = ShelleyDelegationPart.Null
        )

    /** Generator for L2 UTXO sets */
    def genUtxosL2(count: Int = 2): Gen[UtxoSetL2] =
        for {
            outputs <- Gen.listOfN(count, genOutputL2)
            utxoIds <- Gen.listOfN(count, genUtxoIdL2)
        } yield UtxoSet[L2](utxoIds.zip(outputs).toMap)

    /** Generator for a single L2 output */
    def genOutputL2: Gen[OutputL2] =
        for {
            address <- genShelleyAddress
            coin <- Gen.choose(1_000_000L, 10_000_000L)
            value = Value(Coin(coin))
        } yield Output[L2](
          Babbage(
            address = address,
            value = value,
            datumOption = None,
            scriptRef = None
          )
        )

    /** Generator for L2 UTXO ID */
    def genUtxoIdL2: Gen[UtxoId[L2]] =
        for {
            txHash <- genByteStringOfN(32).map(TransactionHash.fromByteString)
            index <- Gen.choose(0, 10)
        } yield UtxoId[L2](TransactionInput(txHash, index))

    /** Generator for version tuple */
    def genVersion: Gen[(BigInt, BigInt)] =
        for {
            major <- Gen.choose(1, 10)
            minor <- Gen.choose(0, 99)
        } yield (BigInt(major), BigInt(minor))

}

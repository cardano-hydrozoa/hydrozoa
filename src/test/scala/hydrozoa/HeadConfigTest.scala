package hydrozoa

import hydrozoa.config.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.Coin
import scalus.uplc.builtin.ByteString
import spire.math.{Rational, UByte}
import spire.syntax.literals.r

//@nowarn("msg=unused value")
//class HeadConfigTest extends AnyFunSuite with ScalaCheckPropertyChecks:
//
//    private val shelleyAddresses = Gen.listOfN(3, arbitrary[ShelleyAddress]).sample.get
//    val address1: AddressL1 = Address[L1](shelleyAddresses(0))
//    val address2: AddressL1 = Address[L1](shelleyAddresses(1))
//    val address3: AddressL1 = Address[L1](shelleyAddresses(2))
//
//    // FIXME: do we have arbitrary for VKs?
//    val vkey1: VerificationKeyBytes = VerificationKeyBytes(ByteString.fromHex("01234567890123456789012345678901234567890123456789012345678901234567"))
//    val vkey2: VerificationKeyBytes = VerificationKeyBytes(ByteString.fromHex("11234567890123456789012345678901234567890123456789012345678901234567"))
//    val vkey3: VerificationKeyBytes = VerificationKeyBytes(ByteString.fromHex("21234567890123456789012345678901234567890123456789012345678901234567"))
//
//    // Valid deposits that meet minimum requirements
//    val validCollateralDeposit = Coin(2_250_000)
//    val validVoteDeposit = Coin(2_140_000) // Above minimum of 3_000_000
//
//    // Valid collective contingency deposits
//    val validCollectiveDeposits = CollectiveContingencyDepositsSection(
//        fallbackTxFee = Coin(1_700_000), // Above worstCaseFixedFeeFallbackTx + 2 * worstCaseVariableFeeFallbackTxPerPeer
//        defaultVoteUtxo = Coin(1_340_000) // Above minAdaBabbageVoteUtxo
//    )
//
//    test("ContingencyDepositsAndEquityShares.apply should succeed with valid shares that sum to 1") {
//        val peerConfigs = Map(
//            UByte(0) -> PeerShare(address1, validCollateralDeposit, validVoteDeposit, r"1/2"),
//            UByte(1) -> PeerShare(address2, validCollateralDeposit, validVoteDeposit, r"1/3"),
//            UByte(2) -> PeerShare(address3, validCollateralDeposit, validVoteDeposit, r"1/6")
//        )
//
//        val result = EquityShares.apply(validCollectiveDeposits, peerConfigs)
//        assert(result.isRight)
//    }
//
//    test("ContingencyDepositsAndEquityShares.apply should fail with shares that don't sum to 1") {
//        val peerConfigs = Map(
//            UByte(0) -> PeerShare(address1, validCollateralDeposit, validVoteDeposit, r"1/2"),
//            UByte(1) -> PeerShare(address2, validCollateralDeposit, validVoteDeposit, r"1/4")
//        )
//
//        val result = EquityShares.apply(validCollectiveDeposits, peerConfigs)
//        assert(result.isLeft)
//        result.left.foreach {
//            case HeadConfigError.SharesMustSumToOne(total) =>
//                assert(total == r"3/4")
//            case _ => fail("Expected SharesMustSumToOne error")
//        }
//    }
//
//    test("ContingencyDepositsAndEquityShares.distribute should correctly distribute coins") {
//        val peerConfigs = Map(
//            UByte(0) -> PeerShare(address1, validCollateralDeposit, validVoteDeposit, r"26/100"),
//            UByte(1) -> PeerShare(address2, validCollateralDeposit, validVoteDeposit, r"43/100"),
//            UByte(2) -> PeerShare(address3, validCollateralDeposit, validVoteDeposit, r"31/100")
//        )
//
//        val contingencyDeposits = EquityShares.apply(validCollectiveDeposits, peerConfigs).toOption.get
//        val totalCoin = Coin(294_300_000)
//        val distribution = contingencyDeposits.distribute(totalCoin)
//
//        assert(distribution.payouts(address1) == Coin(81_698_400))
//        assert(distribution.payouts(address2) == Coin(132_246_200))
//        assert(distribution.payouts(address3) == Coin(96_565_400))
//        assert(distribution.dust == Coin(0))
//    }
//
//    test("ContingencyDepositsAndEquityShares.distribute should handle dust from rounding") {
//        val peerConfigs = Map(
//            UByte(0) -> PeerShare(address1, validCollateralDeposit, validVoteDeposit, r"1/3"),
//            UByte(1) -> PeerShare(address2, validCollateralDeposit, validVoteDeposit, r"1/3"),
//            UByte(2) -> PeerShare(address3, validCollateralDeposit, validVoteDeposit, r"1/3")
//        )
//
//        val contingencyDeposits = EquityShares.apply(validCollectiveDeposits, peerConfigs).toOption.get
//        val totalCoin = Coin(100) // Small amount to test dust calculation
//        val distribution = contingencyDeposits.distribute(totalCoin)
//
//        // Expected: collateral (6M) + vote (4M) + equity share
//        // Each gets 1/3 of 100 = 33, total = 6M + 4M + 33 = 10000033
//        // Dust = 100 - (33 + 33 + 33) = 1
//        assert(distribution.payouts(address1) == Coin(10000033))
//        assert(distribution.payouts(address2) == Coin(10000033))
//        assert(distribution.payouts(address3) == Coin(10000033))
//        assert(distribution.dust == Coin(1))
//    }
//
//    test("HeadConfig.parse should succeed with valid configuration") {
//        val rawConfig = RawConfig(
//            collectiveContingency = validCollectiveDeposits,
//            peers = List(
//                PeerSection(vkey1, address1, validCollateralDeposit, validVoteDeposit, r"1/2"),
//                PeerSection(vkey2, address2, validCollateralDeposit, validVoteDeposit, r"1/2")
//            )
//        )
//
//        val result = HeadConfig.parse(rawConfig)
//        assert(result.isRight)
//
//        result.foreach { config =>
//            assert(config.verificationKeys.size == 2)
//            assert(config.verificationKeys(UByte(0)) == vkey1)
//            assert(config.verificationKeys(UByte(1)) == vkey2)
//        }
//    }
//
//    test("HeadConfig.parse should fail with duplicate verification keys") {
//        val rawConfig = RawConfig(
//            collectiveContingency = validCollectiveDeposits,
//            peers = List(
//                PeerSection(vkey1, address1, validCollateralDeposit, validVoteDeposit, r"1/2"),
//                PeerSection(vkey1, address2, validCollateralDeposit, validVoteDeposit, r"1/2") // Duplicate key
//            )
//        )
//
//        val result = HeadConfig.parse(rawConfig)
//        assert(result.isLeft)
//
//        result.left.foreach {
//            case HeadConfigError.NonUniqueVerificationKey(duplicates) =>
//                assert(duplicates.contains(vkey1))
//            case _ => fail("Expected NonUniqueVerificationKey error")
//        }
//    }
//
//    test("HeadConfig.parse should fail with too many peers") {
//        val manyVKeys = (0 until 255).map { i =>
//            VerificationKeyBytes(ByteString.fromHex(f"${i%256}%02x234567890123456789012345678901234567890123456789012345678901234567"))
//        }.toList
//
//        val manyPeers = manyVKeys.map { vkey =>
//            PeerSection(vkey, address1, validCollateralDeposit, validVoteDeposit, r"1/255")
//        }
//
//        val rawConfig = RawConfig(validCollectiveDeposits, manyPeers)
//        val result = HeadConfig.parse(rawConfig)
//
//        assert(result.isLeft)
//        result.left.foreach {
//            case HeadConfigError.TooManyPeers(count) =>
//                assert(count == 255)
//            case _ => fail("Expected TooManyPeers error")
//        }
//    }
//
//    test("HeadConfig.parse should fail with inconsistent shares") {
//        val rawConfig = RawConfig(
//            collectiveContingency = validCollectiveDeposits,
//            peers = List(
//                PeerSection(vkey1, address1, validCollateralDeposit, validVoteDeposit, r"1/3"),
//                PeerSection(vkey2, address2, validCollateralDeposit, validVoteDeposit, r"1/3") // Sum = 2/3, not 1
//            )
//        )
//
//        val result = HeadConfig.parse(rawConfig)
//        assert(result.isLeft)
//
//        result.left.foreach {
//            case HeadConfigError.SharesMustSumToOne(total) =>
//                assert(total == r"2/3")
//            case _ => fail("Expected SharesMustSumToOne error")
//        }
//    }
//
//    test("HeadConfig.parse should fail with too small collateral deposit") {
//        val tooSmallCollateral = Coin(100) // Way below minimum requirements
//        val rawConfig = RawConfig(
//            collectiveContingency = validCollectiveDeposits,
//            peers = List(
//                PeerSection(vkey1, address1, tooSmallCollateral, validVoteDeposit, r"1/2"),
//                PeerSection(vkey2, address2, validCollateralDeposit, validVoteDeposit, r"1/2")
//            )
//        )
//
//        val result = HeadConfig.parse(rawConfig)
//        assert(result.isLeft)
//
//        result.left.foreach {
//            case HeadConfigError.TooSmallCollateralDeposit(peer, minimal, actual) =>
//                assert(peer == UByte(0))
//                assert(actual == tooSmallCollateral)
//            case _ => fail("Expected TooSmallCollateralDeposit error")
//        }
//    }
//
//    test("HeadConfig.parse should fail with too small vote deposit") {
//        val tooSmallVoteDeposit = Coin(100) // Way below minimum requirements
//        val rawConfig = RawConfig(
//            collectiveContingency = validCollectiveDeposits,
//            peers = List(
//                PeerSection(vkey1, address1, validCollateralDeposit, tooSmallVoteDeposit, r"1/2"),
//                PeerSection(vkey2, address2, validCollateralDeposit, validVoteDeposit, r"1/2")
//            )
//        )
//
//        val result = HeadConfig.parse(rawConfig)
//        assert(result.isLeft)
//
//        result.left.foreach {
//            case HeadConfigError.TooSmallVoteDeposit(peer, minimal, actual) =>
//                assert(peer == UByte(0))
//                assert(actual == tooSmallVoteDeposit)
//            case _ => fail("Expected TooSmallVoteDeposit error")
//        }
//    }

package hydrozoa

import hydrozoa.config.{EquityShares, HeadConfig, HeadConfigError, PeerSection, RawConfig}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.builtin.ByteString
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.Coin
import spire.math.{Rational, UByte}
import spire.syntax.literals.r

@nowarn("msg=unused value")
class HeadConfigTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private val shelleyAddresses = Gen.listOfN(3, arbitrary[ShelleyAddress]).sample.get
    val address1: AddressL1 = Address[L1](shelleyAddresses(0))
    val address2: AddressL1 = Address[L1](shelleyAddresses(1))
    val address3: AddressL1 = Address[L1](shelleyAddresses(2))

    // FIXME: do we have arbitrary for VKs?
    val vkey1: VerificationKeyBytes = VerificationKeyBytes(ByteString.fromHex("01234567890123456789012345678901234567890123456789012345678901234567"))
    val vkey2: VerificationKeyBytes = VerificationKeyBytes(ByteString.fromHex("11234567890123456789012345678901234567890123456789012345678901234567"))
    val vkey3: VerificationKeyBytes = VerificationKeyBytes(ByteString.fromHex("21234567890123456789012345678901234567890123456789012345678901234567"))

    test("EquityShares.apply should succeed with valid shares that sum to 1") {
        val shares = List(
            (address1, r"1/2"),
            (address2, r"1/3"),
            (address3, r"1/6")
        )
        
        val result = EquityShares.apply(shares)
        assert(result.isRight)
    }

    test("EquityShares.apply should fail with shares that don't sum to 1") {
        val shares = List(
            (address1, r"1/2"),
            (address2, r"1/4")
        )
        
        val result = EquityShares.apply(shares)
        assert(result.isLeft)
        result.left.foreach { error =>
            assert(error.total == r"3/4")
        }
    }

    test("EquityShares.distribute should correctly distribute coins") {
        val shares = List(
            (address1, r"1/2"),
            (address2, r"1/3"),
            (address3, r"1/6")
        )
        
        val equityShares = EquityShares.apply(shares).toOption.get
        val totalCoin = Coin(600)
        val distribution = equityShares.distribute(totalCoin)
        
        // Expected: 300, 200, 100 with no dust
        assert(distribution.shares(address1) == Coin(300))
        assert(distribution.shares(address2) == Coin(200))
        assert(distribution.shares(address3) == Coin(100))
        assert(distribution.dust == Coin(0))
    }

    test("EquityShares.distribute should handle dust from rounding") {
        val shares = List(
            (address1, r"1/3"),
            (address2, r"1/3"),
            (address3, r"1/3")
        )
        
        val equityShares = EquityShares.apply(shares).toOption.get
        val totalCoin = Coin(100)
        val distribution = equityShares.distribute(totalCoin)
        
        // Expected: 33, 33, 33 with 1 dust
        assert(distribution.shares(address1) == Coin(33))
        assert(distribution.shares(address2) == Coin(33))
        assert(distribution.shares(address3) == Coin(33))
        assert(distribution.dust == Coin(1))
    }

    test("HeadConfig.parse should succeed with valid configuration") {
        val rawConfig = RawConfig(List(
            PeerSection(vkey1, address1, r"1/2"),
            PeerSection(vkey2, address2, r"1/2")
        ))
        
        val result = HeadConfig.parse(rawConfig)
        assert(result.isRight)
        
        result.foreach { config =>
            assert(config.peers.size == 2)
            assert(config.peers(UByte(0)) == vkey1)
            assert(config.peers(UByte(1)) == vkey2)
        }
    }

    test("HeadConfig.parse should fail with duplicate verification keys") {
        val rawConfig = RawConfig(List(
            PeerSection(vkey1, address1, r"1/2"),
            PeerSection(vkey1, address2, r"1/2") // Duplicate key
        ))
        
        val result = HeadConfig.parse(rawConfig)
        assert(result.isLeft)
        
        result.left.foreach {
            case HeadConfigError.NonUniqueVerificationKey(duplicates) =>
                assert(duplicates.contains(vkey1))
            case _ => fail("Expected NonUniqueVerificationKey error")
        }
    }

    test("HeadConfig.parse should fail with too many peers") {
        val manyVKeys = (0 until 255).map { i =>
            VerificationKeyBytes(ByteString.fromHex(f"${i%256}%02x234567890123456789012345678901234567890123456789012345678901234567"))
        }.toList

        val manyPeers = manyVKeys.map { vkey =>
            PeerSection(vkey, address1, r"1/255")
        }
        
        val rawConfig = RawConfig(manyPeers)
        val result = HeadConfig.parse(rawConfig)
        
        assert(result.isLeft)
        result.left.foreach {
            case HeadConfigError.TooManyPeers(count) =>
                assert(count == 255)
            case _ => fail("Expected TooManyPeers error")
        }
    }

    test("HeadConfig.parse should fail with inconsistent shares") {
        val rawConfig = RawConfig(List(
            PeerSection(vkey1, address1, r"1/3"),
            PeerSection(vkey2, address2, r"1/3") // Sum = 2/3, not 1
        ))
        
        val result = HeadConfig.parse(rawConfig)
        assert(result.isLeft)
        
        result.left.foreach {
            case HeadConfigError.NonConsistentShares(total) =>
                assert(total == r"2/3")
            case _ => fail("Expected NonConsistentShares error")
        }
    }

package hydrozoa.multisig.persistence.codec

import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigTreasuryUtxo}
import org.scalacheck.Arbitrary
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{AddrKeyHash, AssetName, Coin, TransactionInput, Value}
import scalus.uplc.builtin.ByteString

/** A small, deterministic-ish [[MultisigTreasuryUtxo]] sample for codec / persistence tests.
  *
  * Built from Scalus arbitraries for the on-chain identifiers (`TransactionInput`,
  * `AddrKeyHash`) and fixed bytes everywhere else — semantic validity isn't the point, the
  * tests using this fixture only care about byte-level round-trip.
  */
object TreasuryFixture:
    val sampleTreasury: MultisigTreasuryUtxo =
        val tokenNameBytes = ByteString.fromArray(Array.fill[Byte](16)(0xab.toByte))
        val utxoId: TransactionInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val addrKeyHash: AddrKeyHash = Arbitrary.arbitrary[AddrKeyHash].sample.get
        val address = ShelleyAddress(
          network = Network.Testnet,
          payment = ShelleyPaymentPart.Key(addrKeyHash),
          delegation = ShelleyDelegationPart.Null
        )
        val datum = MultisigTreasuryUtxo.Datum(
          commit = ByteString.fromArray(Array.fill[Byte](48)(0x42.toByte)),
          versionMajor = BigInt(7)
        )
        MultisigTreasuryUtxo(
          treasuryTokenName = AssetName(tokenNameBytes),
          utxoId = utxoId,
          address = address,
          datum = datum,
          value = Value(Coin(5_000_000L)),
          equity = Equity(Coin(1_000_000L)).getOrElse(
            throw new IllegalStateException("expected Some(Equity)")
          )
        )

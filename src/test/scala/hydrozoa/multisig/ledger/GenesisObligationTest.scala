package hydrozoa.multisig.ledger

import cats.data.NonEmptyList
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetworkGen.given_Arbitrary_CardanoNetwork
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import io.bullet.borer.Cbor
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import scalus.cardano.address.ArbitraryInstances.given_Arbitrary_ShelleyAddress
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, OriginalCborByteArray, TransactionOutput}
import test.Generators.Hydrozoa.genGenesisObligation

object GenesisObligationTest extends Properties("Genesis Obligation Properties"):

    val _ = property("Cbor Round Trip") = Prop.forAll(
      arbitrary[CardanoNetwork]
          .flatMap(cardanoNetwork => arbitrary[ShelleyAddress].map(cardanoNetwork -> _))
          .flatMap((cardanoNetwork, address) =>
              Gen.nonEmptyListOf(
                genGenesisObligation(cardanoNetwork, address, minimumCoin = Coin.ada(5))
              ).map(NonEmptyList.fromListUnsafe)
          )
    )(genesisObligations =>
        val bytes = Cbor
            .encode(
              genesisObligations.toList
                  .map(_.toTransactionOutput.asInstanceOf[TransactionOutput])
            )
            .toByteArray

        given OriginalCborByteArray = OriginalCborByteArray(bytes)

        val roundTrippedList: List[TransactionOutput] =
            Cbor
                .decode(bytes)
                .to[List[TransactionOutput]]
                .value

        val roundTripped = NonEmptyList.fromListUnsafe(
          roundTrippedList.map(GenesisObligation.fromTransactionOutput)
        )

        genesisObligations.map(Right(_)) == roundTripped
    )

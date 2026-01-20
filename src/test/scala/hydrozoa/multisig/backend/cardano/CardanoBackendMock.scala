package hydrozoa.multisig.backend.cardano

import cats.arrow.FunctionK
import cats.data.State
import cats.effect.{IO, Ref}
import cats.syntax.all.catsSyntaxFlatMapOps
import cats.~>
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.multisig.backend.cardano.CardanoBackend.GetTxInfo
import hydrozoa.{L1, Output, UtxoIdL1, UtxoSet, UtxoSetL1}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Mutator
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State as LedgerState}
import scalus.cardano.ledger.{AssetName, PolicyId, Transaction, TransactionHash, Utxos, Value}

final case class MockState(
    ledgerState: LedgerState,
    knownTxs: Set[TransactionHash] = Set.empty
)

object MockState:
    def apply(initialUtxos: Utxos): MockState =
        MockState(
          ledgerState = LedgerState(utxos = initialUtxos)
        )

type MockStateF[A] = State[MockState, A]

// It should be ReaderT over those vals, though I don't think it buys anything but extra lifts.
class CardanoBackendMock private (
    private val mutator: Mutator,
    private val context: Context
) extends CardanoBackend[MockStateF] {
    import CardanoBackend.*
    import State.*

    override def utxosAt(
        address: ShelleyAddress
    ): State[MockState, Either[CardanoBackend.Error, UtxoSetL1]] = {
        println("utxosAt")
        for {
            state: MockState <- get
            ret: UtxoSetL1 = UtxoSet(
              state.ledgerState.utxos
                  .filter((_, o) => o.address == address)
                  .map((in, out) => UtxoIdL1(in) -> Output[L1](out.asInstanceOf[Babbage]))
            )
        } yield Right(ret)
    }

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): State[MockState, Either[CardanoBackend.Error, UtxoSetL1]] = {

        extension (v: Value)
            def contain(asset: (PolicyId, AssetName)): Boolean =
                v.assets.assets.get(asset._1).flatMap(_.get(asset._2)).isDefined

        for {
            state: MockState <- get
            ret: UtxoSetL1 = UtxoSet(
              state.ledgerState.utxos
                  .filter((_, o) => o.address == address && o.value.contain(asset))
                  .map((in, out) => UtxoIdL1(in) -> Output[L1](out.asInstanceOf[Babbage]))
            )

        } yield Right(ret)
    }

    override def getTxInfo(
        txHash: TransactionHash
    ): State[MockState, Either[CardanoBackend.Error, GetTxInfo.Response]] = for {
        state: MockState <- get
        ret = GetTxInfo.Response(state.knownTxs.contains(txHash))
    } yield Right(ret)

    override def submitTx(tx: Transaction): State[MockState, Either[CardanoBackend.Error, Unit]] = {
        println(s"submitTx: ${tx.id}")
        println(s"submitTx: ${HexUtil.encodeHexString(tx.toCbor)}")

        for {
            state: MockState <- get
            ret <-
                // TODO: is this what we want to have?
                if state.knownTxs.contains(tx.id)
                then pure(Right(()))
                else
                    mutator.transit(context, state.ledgerState, tx) match {
                        case Left(err) =>
                            pure(Left(Error.InvalidTx(err.explain)))
                        case Right(newLedgerState) =>
                            set(
                              state.copy(
                                ledgerState = newLedgerState,
                                knownTxs = state.knownTxs + tx.id
                              )
                            ) >> pure(Right(()))
                    }
        } yield ret
    }
}

object CardanoBackendMock {

    /** Creates a mock lifted to IO. */
    def mockIO(
        initialState: MockState,
        mutator: Mutator = CardanoMutator,
        context: Context = Context.testMainnet()
    ): IO[CardanoBackend[IO]] =
        Ref.of[IO, MockState](initialState).map { stateRef =>
            val mock = new CardanoBackendMock(mutator, context)

            val transformer: MockStateF ~> IO =
                new FunctionK[MockStateF, IO] {
                    def apply[A](state: State[MockState, A]): IO[A] =
                        stateRef.modify(s => state.run(s).value)
                }

            new CardanoBackend[IO] {
                override def utxosAt(
                    address: ShelleyAddress
                ): IO[Either[CardanoBackend.Error, UtxoSetL1]] =
                    transformer(mock.utxosAt(address))

                override def utxosAt(
                    address: ShelleyAddress,
                    asset: (PolicyId, AssetName)
                ): IO[Either[CardanoBackend.Error, UtxoSetL1]] =
                    transformer(mock.utxosAt(address, asset))

                override def getTxInfo(
                    txHash: TransactionHash
                ): IO[Either[CardanoBackend.Error, GetTxInfo.Response]] =
                    transformer(mock.getTxInfo(txHash))

                override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] =
                    transformer(mock.submitTx(tx))
            }
        }
}

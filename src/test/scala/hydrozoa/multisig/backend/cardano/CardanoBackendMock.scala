package hydrozoa.multisig.backend.cardano

import cats.arrow.FunctionK
import cats.data.State
import cats.effect.{IO, Ref}
import cats.syntax.all.catsSyntaxFlatMapOps
import cats.~>
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.backend.cardano.CardanoBackend.GetTxInfo
import hydrozoa.{L1, Output, UtxoIdL1, UtxoSet, UtxoSetL1}
import monocle.Focus.focus
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.STS.Mutator
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State as LedgerState}
import scalus.cardano.ledger.{AssetName, PolicyId, RedeemerTag, Slot, Transaction, TransactionHash, Utxos, Value}

final case class MockState(
    ledgerState: LedgerState,
    currentSlot: Slot = Slot(0),
    knownTxs: Set[TransactionHash] = Set.empty,
    submittedTxs: List[Transaction] = List.empty
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
    private val mkContext: Long => Context
) extends CardanoBackend[MockStateF] {
    import CardanoBackend.*
    import State.*

    override def utxosAt(
        address: ShelleyAddress
    ): MockStateF[Either[CardanoBackend.Error, UtxoSetL1]] = {
        println("utxosAt")
        for {
            state: MockState <- get
            ret: UtxoSetL1 = UtxoSet(
              state.ledgerState.utxos
                  .filter((_, o) => o.address == address)
                  .map((in, out) => UtxoIdL1(in) -> Output[L1](out))
            )
        } yield Right(ret)
    }

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): MockStateF[Either[CardanoBackend.Error, UtxoSetL1]] = {

        extension (v: Value)
            def contain(asset: (PolicyId, AssetName)): Boolean =
                v.assets.assets.get(asset._1).flatMap(_.get(asset._2)).isDefined

        for {
            state: MockState <- get
            ret: UtxoSetL1 = UtxoSet(
              state.ledgerState.utxos
                  .filter((_, o) => o.address == address && o.value.contain(asset))
                  .map((in, out) => UtxoIdL1(in) -> Output[L1](out))
            )

        } yield Right(ret)
    }

    override def getTxInfo(
        txHash: TransactionHash
    ): MockStateF[Either[CardanoBackend.Error, GetTxInfo.Response]] = for {
        state: MockState <- get
        isKnown = state.knownTxs.contains(txHash)
        spendingRedeemersData = state.submittedTxs
            .find(_.id == txHash)
            .fold(List.empty) { tx =>
                tx.witnessSet.redeemers.fold(List.empty)(
                  _.value.toSeq
                      .filter(_.tag == RedeemerTag.Spend)
                      .map(_.data)
                      .toList
                )
            }
        ret = GetTxInfo.Response(isKnown, spendingRedeemersData)
    } yield Right(ret)

    override def assetTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): MockStateF[Either[Error, List[TransactionHash]]] = {

        extension (v: Value)
            def contains(asset: (PolicyId, AssetName)): Boolean =
                v.assets.assets.get(asset._1).flatMap(_.get(asset._2)).isDefined

        def txContainsAsset(tx: Transaction, asset: (PolicyId, AssetName)): Boolean =
            tx.body.value.outputs.exists(_.value.value.contains(asset)) ||
                tx.body.value.mint
                    .fold(false)(_.assets.get(asset._1).flatMap(_.get(asset._2)).isDefined)

        for {
            state: MockState <- get
            // Transactions are stored oldest first, reverse to get newest first
            allTxsReversed = state.submittedTxs.reverse
            // Take transactions after the 'after' transaction (excluding it)
            txsAfter = allTxsReversed.takeWhile(_.id != after)
            // Filter for transactions containing the asset
            result = txsAfter.filter(txContainsAsset(_, asset)).map(_.id)
        } yield Right(result)
    }

    override def submitTx(tx: Transaction): MockStateF[Either[CardanoBackend.Error, Unit]] = {
        println(s"submitTx: ${tx.id}")
        // println(s"submitTx: ${HexUtil.encodeHexString(tx.toCbor)}")

        for {
            state <- get[MockState]
            _ = println(s"utxos count: ${state.ledgerState.utxos.values.size}")
            _ = println(
              s"missing utxos: ${tx.body.value.inputs.toSet &~ state.ledgerState.utxos.keySet}"
            )
            // _ = println(s"tx utxos: ${state.ledgerState.utxos.filter((i, _) => tx.body.value.inputs.toSet.contains(i))}")

            ret <-
                // TODO: is this what we want to have?
                if state.knownTxs.contains(tx.id)
                then pure(Right(()))
                else
                    mutator.transit(
                      mkContext(state.currentSlot.slot),
                      state.ledgerState,
                      tx
                    ) match {
                        case Left(err) =>
                            pure(Left(Error.InvalidTx(err.explain)))
                        // TODO: Why doesn't mutator touch the context?
                        case Right(newLedgerState) =>
                            val newState: MockState = state.copy(
                              ledgerState = newLedgerState,
                              knownTxs = state.knownTxs + tx.id,
                              submittedTxs = state.submittedTxs :+ tx
                            )
                            set[MockState](newState) >> pure(Right(()))
                    }
        } yield ret
    }

    def setSlot(currentSlot: Slot): MockStateF[Unit] = {
        modify[MockState](s => s.focus(_.currentSlot).replace(currentSlot))
    }

}

object CardanoBackendMock {

    /** Creates a mock lifted to IO.
      *
      * NB: the slot is updated to the current time every before the action is lifted.
      */
    def mockIO(
        initialState: MockState,
        mutator: Mutator = CardanoMutator,
        mkContext: Long => Context = Context.testMainnet
    ): IO[CardanoBackend[IO]] =
        Ref.of[IO, MockState](initialState).map { stateRef =>
            val mock = new CardanoBackendMock(mutator, mkContext)
            // TODO: this is awkward, but this requires the mending of Scalus
            //  - Context is not a case class
            //  - Context is not returned by CardanoMutator
            val slotConfig = mkContext(0).slotConfig

            val transformer: MockStateF ~> IO =
                new FunctionK[MockStateF, IO] {
                    def apply[A](state: State[MockState, A]): IO[A] = for {
                        now <- IO.realTimeInstant
                        currentSlot = QuantizedInstant.apply(slotConfig, now).toSlot
                        ret <- stateRef
                            .modify(s => (mock.setSlot(currentSlot) >> state).run(s).value)
                    } yield ret
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

                override def assetTxs(
                    asset: (PolicyId, AssetName),
                    after: TransactionHash
                ): IO[Either[CardanoBackend.Error, List[TransactionHash]]] =
                    transformer(mock.assetTxs(asset, after))

                override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] =
                    transformer(mock.submitTx(tx))
            }
        }
}

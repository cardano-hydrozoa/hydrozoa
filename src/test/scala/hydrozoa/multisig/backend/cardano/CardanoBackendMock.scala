package hydrozoa.multisig.backend.cardano

import cats.arrow.FunctionK
import cats.data.State
import cats.effect.{IO, Ref}
import cats.syntax.all.catsSyntaxFlatMapOps
import cats.~>
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error
import monocle.Focus.focus
import scalus.builtin.Data
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.STS.Mutator
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State as LedgerState}
import scalus.cardano.ledger.{AssetName, PolicyId, ProtocolParams, RedeemerTag, Slot, Transaction, TransactionHash, Utxos, Value}

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
    ): MockStateF[Either[CardanoBackend.Error, Utxos]] = {
        // println("utxosAt")
        for {
            state: MockState <- get
            ret: Utxos =
                state.ledgerState.utxos
                    .filter((_, o) => o.address == address)
                    .map((in, out) => in -> out)
        } yield Right(ret)
    }

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): MockStateF[Either[CardanoBackend.Error, Utxos]] = {

        extension (v: Value)
            def contain(asset: (PolicyId, AssetName)): Boolean =
                v.assets.assets.get(asset._1).flatMap(_.get(asset._2)).isDefined

        for {
            state: MockState <- get
            ret: Utxos =
                state.ledgerState.utxos
                    .filter((_, o) => o.address == address && o.value.contain(asset))
                    .map((in, out) => in -> out)

        } yield Right(ret)
    }

    override def isTxKnown(
        txHash: TransactionHash
    ): MockStateF[Either[CardanoBackend.Error, Boolean]] = for {
        state: MockState <- get
        isKnown = state.knownTxs.contains(txHash)
    } yield Right(isKnown)

    override def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): MockStateF[Either[Error, List[(TransactionHash, Data)]]] = {

        extension (v: Value)
            def contains(asset: (PolicyId, AssetName)): Boolean =
                v.assets.assets.get(asset._1).flatMap(_.get(asset._2)).isDefined

        def continuingInputRedeemer(
            tx: Transaction,
            asset: (PolicyId, AssetName),
            utxos: Utxos
        ): Option[Data] = {
            // Find the input index that contains the asset
            val inputWithAssetIdx = tx.body.value.inputs.toSeq.zipWithIndex
                .find { (input, _) =>
                    utxos.get(input).exists(_.value.contains(asset))
                }
                .map(_._2)

            // Check if there's an output with the asset (continuing pattern)
            val hasOutputWithAsset = tx.body.value.outputs.exists(_.value.value.contains(asset))

            // Extract the redeemer for the spending input
            for {
                inputIx <- inputWithAssetIdx
                _ <- if hasOutputWithAsset then Some(()) else None
                redeemers <- tx.witnessSet.redeemers.map(_.value)
                redeemer <- redeemers.toSeq.find { r =>
                    r.tag == RedeemerTag.Spend && r.index.toInt == inputIx
                }
            } yield redeemer.data
        }

        for {
            state: MockState <- get
            // Transactions are stored oldest first, reverse to get newest first
            allTxsReversed = state.submittedTxs.reverse
            // Take transactions after the 'after' transaction (excluding it)
            txsAfter = allTxsReversed.takeWhile(_.id != after)
            // Extract transactions with continuing inputs (input + output with asset)
            result = txsAfter.flatMap(tx =>
                continuingInputRedeemer(tx, asset, state.ledgerState.utxos).map(redeemer =>
                    tx.id -> redeemer
                )
            )
        } yield Right(result)
    }

    override def submitTx(tx: Transaction): MockStateF[Either[CardanoBackend.Error, Unit]] = {
        // println(s"submitTx: ${tx.id}")
        // println(s"submitTx: ${HexUtil.encodeHexString(tx.toCbor)}")

        for {
            state <- get[MockState]
            // _ = println(s"utxos count: ${state.ledgerState.utxos.values.size}")
            // _ = println(
            //  s"missing utxos: ${tx.body.value.inputs.toSet &~ state.ledgerState.utxos.keySet}"
            // )
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

    def latestParams: MockStateF[Either[Error, ProtocolParams]] = {
        // As long as paramters don't depend on the slot number it's fine
        State.pure(Right(mkContext(0).env.params))
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
                        // _ <- IO.println(s"transformer now=$now")
                        // _ <- IO.println(s"transformer slotConfig=$slotConfig")
                        currentSlot = QuantizedInstant.apply(slotConfig, now).toSlot
                        ret <- stateRef
                            .modify(s => (mock.setSlot(currentSlot) >> state).run(s).value)
                    } yield ret
                }

            new CardanoBackend[IO] {
                override def utxosAt(
                    address: ShelleyAddress
                ): IO[Either[CardanoBackend.Error, Utxos]] =
                    transformer(mock.utxosAt(address))

                override def utxosAt(
                    address: ShelleyAddress,
                    asset: (PolicyId, AssetName)
                ): IO[Either[CardanoBackend.Error, Utxos]] =
                    transformer(mock.utxosAt(address, asset))

                override def isTxKnown(
                    txHash: TransactionHash
                ): IO[Either[CardanoBackend.Error, Boolean]] =
                    transformer(mock.isTxKnown(txHash))

                override def lastContinuingTxs(
                    asset: (PolicyId, AssetName),
                    after: TransactionHash
                ): IO[Either[CardanoBackend.Error, List[(TransactionHash, Data)]]] =
                    transformer(mock.lastContinuingTxs(asset, after))

                override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] =
                    transformer(mock.submitTx(tx))

                def latestParams: IO[Either[Error, ProtocolParams]] =
                    transformer(mock.latestParams)
            }
        }
}

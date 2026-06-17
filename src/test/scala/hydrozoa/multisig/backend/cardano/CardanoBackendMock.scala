package hydrozoa.multisig.backend.cardano

import cats.arrow.FunctionK
import cats.data.State
import cats.effect.{IO, Ref}
import cats.syntax.all.catsSyntaxFlatMapOps
import cats.~>
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error
import monocle.Focus.focus
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.STS.Mutator
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State as LedgerState}
import scalus.cardano.ledger.{AssetName, PolicyId, ProtocolParams, RedeemerTag, Slot, Transaction, TransactionHash, TransactionInput, Utxo, Utxos, Value}
import scalus.uplc.builtin.Data

val logger = Logging.logger("test.CardanoBackendMock")

final case class MockState(
    ledgerState: LedgerState,
    currentSlot: Slot = Slot(0),
    knownTxs: Set[TransactionHash] = Set.empty,
    submittedTxs: List[(Utxos, Transaction)] = List.empty
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
    import State.*

    override def resolve(
        input: TransactionInput
    ): MockStateF[Either[CardanoBackend.Error, Option[Utxo]]] =
        for {
            state <- get[MockState]
            res = state.ledgerState.utxos.get(input).map(output => Utxo(input, output))
        } yield Right(res)

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
        _ = logger.trace(s"isTxKnown: looking for tx ${txHash} in state.knownTxs ${state.knownTxs}")
    } yield Right(isKnown)

    override def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): MockStateF[Either[Error, List[(TransactionHash, Data, Data)]]] = {

        extension (v: Value)
            def contains(asset: (PolicyId, AssetName)): Boolean =
                v.assets.assets.get(asset._1).flatMap(_.get(asset._2)).isDefined

        def continuingInputRedeemerAndOutputDatum(
            tx: Transaction,
            asset: (PolicyId, AssetName),
            utxos: Utxos
        ): Option[(Data, Data)] = {
            // Find the input index that contains the asset
            val inputWithAssetIdx = tx.body.value.inputs.toSeq.zipWithIndex
                .find { (input, _) =>
                    utxos.get(input).exists(_.value.contains(asset))
                }
                .map(_._2)

            // Extract the redeemer for the spending input
            for {
                // Check if there's an output with the asset (continuing pattern)
                outputDatum <- for {
                    outputWithAsset <- tx.body.value.outputs.find(_.value.value.contains(asset))
                    datum <- outputWithAsset.value.inlineDatum
                } yield datum
                inputIx <- inputWithAssetIdx
                redeemers <- tx.witnessSet.redeemers.map(_.value)
                redeemer <- redeemers.toSeq.find { r =>
                    r.tag == RedeemerTag.Spend && r.index.toInt == inputIx
                }
            } yield (redeemer.data, outputDatum)
        }

        for {
            state: MockState <- get
            // Transactions are stored oldest first, reverse to get newest first
            allTxsReversed = state.submittedTxs.reverse
            // Take transactions after the 'after' transaction (excluding it)
            txsAfter = allTxsReversed.takeWhile(_._2.id != after)
            // Extract transactions with continuing inputs (input + output with asset).
            // Use the pre-application UTxO snapshot so historical inputs (already consumed)
            // can still be found — mirrors the Blockfrost implementation's historical query.
            result = txsAfter.flatMap { (preUtxos, tx) =>
                continuingInputRedeemerAndOutputDatum(tx, asset, preUtxos).map((r, d) =>
                    (tx.id, r, d)
                )
            }
        } yield Right(result)
    }

    override def submitTx(tx: Transaction): MockStateF[Either[CardanoBackend.Error, Unit]] = {
        logger.debug(s"submitTx: ${tx.id}")
        logger.trace(s"submitTx: ${HexUtil.encodeHexString(tx.toCbor)}")

        for {
            state <- get[MockState]

            _ = logger.trace(s"utxos count: ${state.ledgerState.utxos.values.size}")
            _ = logger.trace(
              s"missing utxos: ${tx.body.value.inputs.toSet &~ state.ledgerState.utxos.keySet}"
            )
            _ = logger.trace(s"tx utxos: ${state.ledgerState.utxos
                    .filter((i, _) => tx.body.value.inputs.toSet.contains(i))}")

            ret <-
                // TODO: is this what we want to have?
                // TODO: Maybe in some cases it would be nice to know that tx has been submitted more than once
                if state.knownTxs.contains(tx.id)
                then
                    logger.debug(s"tx ${tx.id} is already known, do nothing")
                    pure(Right(()))
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
                              submittedTxs = state.submittedTxs :+ (state.ledgerState.utxos, tx)
                            )
                            set[MockState](newState) >> pure(Right(()))
                    }
        } yield ret
    }

    def fetchLatestParams: MockStateF[Either[Error, ProtocolParams]] = {
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
        mockIOWithSnapshot(initialState, mutator, mkContext).map(_._1)

    /** Like [[mockIO]] but also returns an `IO[Utxos]` that reads the current UTxO set from the
      * shared state ref. Useful for inspecting the terminal ledger state after actors finish.
      */
    def mockIOWithSnapshot(
        initialState: MockState,
        mutator: Mutator = CardanoMutator,
        mkContext: Long => Context = Context.testMainnet
    ): IO[(CardanoBackend[IO], IO[Utxos])] = {
        logger.info("Running Cardano backend mock in IO...")
        logger.debug(s"initial utxo ids: ${initialState.ledgerState.utxos.map(_._1)}")

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

            val backend: CardanoBackend[IO] = new CardanoBackend[IO] {
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
                ): IO[Either[CardanoBackend.Error, List[(TransactionHash, Data, Data)]]] =
                    transformer(mock.lastContinuingTxs(asset, after))

                override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] =
                    transformer(mock.submitTx(tx))

                def fetchLatestParams: IO[Either[Error, ProtocolParams]] =
                    transformer(mock.fetchLatestParams)

                override def resolve(input: TransactionInput): IO[Either[Error, Option[Utxo]]] =
                    transformer(mock.resolve(input))
            }

            val snapshot: IO[Utxos] = stateRef.get.map(_.ledgerState.utxos)

            (backend, snapshot)
        }
    }
}

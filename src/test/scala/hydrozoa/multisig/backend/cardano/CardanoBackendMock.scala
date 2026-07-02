package hydrozoa.multisig.backend.cardano

import cats.arrow.FunctionK
import cats.data.State
import cats.effect.{IO, Ref}
import cats.implicits.toContravariantOps
import cats.syntax.all.catsSyntaxFlatMapOps
import cats.syntax.foldable.*
import cats.~>
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.{ContraTracer, LogEvent, Slf4jTracer}
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import monocle.Focus.focus
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.STS.Mutator
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State as LedgerState}
import scalus.cardano.ledger.{AssetName, PolicyId, ProtocolParams, RedeemerTag, Slot, Transaction, TransactionHash, TransactionInput, Utxo, Utxos, Value}
import scalus.uplc.builtin.Data

final case class MockState(
    ledgerState: LedgerState,
    currentSlot: Slot = Slot(0),
    knownTxs: Set[TransactionHash] = Set.empty,
    submittedTxs: List[(Utxos, Transaction)] = List.empty,
    pendingLogs: List[LogEvent] = Nil
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

    private val log: ContraTracer[MockStateF, CardanoBackendEvent] =
        ContraTracer
            .emit[MockStateF, LogEvent](ev =>
                State.modify[MockState](s => s.copy(pendingLogs = s.pendingLogs :+ ev))
            )
            .contramap(CardanoBackendEventFormat.humanFormat)

    protected def tracer: ContraTracer[MockStateF, CardanoBackendEvent] = log

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
        _ <- log.traceWith(CardanoBackendEvent.IsTxKnownChecked(txHash, state.knownTxs))
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

    override def submitTx(etx: EnrichedTx[?]): MockStateF[Either[CardanoBackend.Error, Unit]] =
        val tx = etx.tx
        for {
            _ <- log.traceWith(CardanoBackendEvent.SubmitTxReceived(tx.id))
            _ <- log.traceWith(
              CardanoBackendEvent.SubmitTxCbor(HexUtil.encodeHexString(tx.toCbor))
            )
            state <- get[MockState]
            _ <- log.traceWith(
              CardanoBackendEvent.SubmitTxUtxoSetSize(state.ledgerState.utxos.values.size)
            )
            _ <- log.traceWith(
              CardanoBackendEvent.SubmitTxMissingInputs(
                tx.body.value.inputs.toSet &~ state.ledgerState.utxos.keySet
              )
            )
            _ <- log.traceWith(
              CardanoBackendEvent.SubmitTxRelevantUtxos(
                state.ledgerState.utxos.filter((i, _) => tx.body.value.inputs.toSet.contains(i))
              )
            )
            ret <-
                // TODO: is this what we want to have?
                // TODO: Maybe in some cases it would be nice to know that tx has been submitted more than once
                if state.knownTxs.contains(tx.id)
                then
                    log.traceWith(CardanoBackendEvent.SubmitTxAlreadyKnown(tx.id)) >> pure(
                      Right(())
                    )
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
        val ioLog: ContraTracer[IO, CardanoBackendEvent] =
            Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)

        for {
            _ <- ioLog.traceWith(CardanoBackendEvent.MockStarted)
            _ <- ioLog.traceWith(
              CardanoBackendEvent.MockInitialUtxos(initialState.ledgerState.utxos.keys)
            )
            stateRef <- Ref.of[IO, MockState](initialState)
        } yield {
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
                        retAndLogs <- stateRef.modify { s =>
                            val (newState, result) =
                                (mock.setSlot(currentSlot) >> state).run(s).value
                            (newState.copy(pendingLogs = Nil), (result, newState.pendingLogs))
                        }
                        (ret, pendingLogs) = retAndLogs
                        _ <- pendingLogs.traverse_(ev => Slf4jTracer.sink.traceWith(ev))
                    } yield ret
                }

            val backend: CardanoBackend[IO] = new CardanoBackend[IO] {
                protected def tracer: ContraTracer[IO, CardanoBackendEvent] = ioLog

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

                override def submitTx(etx: EnrichedTx[?]): IO[Either[CardanoBackend.Error, Unit]] =
                    transformer(mock.submitTx(etx))

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

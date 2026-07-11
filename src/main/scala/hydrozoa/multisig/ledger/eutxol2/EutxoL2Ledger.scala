package hydrozoa.multisig.ledger.eutxol2

import cats.*
import cats.data.*
import cats.effect.{Async, IO, Ref}
import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.store.{InMemoryL2Store, L2Snapshot, L2Store}
import hydrozoa.multisig.ledger.eutxol2.tx.{L2Genesis, L2Tx}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey, EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import hydrozoa.multisig.ledger.l2.*
import hydrozoa.multisig.ledger.l2.L2CommandNumber.increment
import hydrozoa.multisig.ledger.l2.L2LedgerCommand.RegisterDeposit
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import scala.collection.immutable.TreeMap
import scala.util.Try
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

extension (ti: TransactionInput) {
    // Technically, this is partial -- but with the current cbor codec of TransactionInput
    // and invariants enforced on EvacuationKey.apply (36 bytes), this should not throw
    def toEvacuationKey: EvacuationKey = EvacuationKey(
      ByteString.fromArray(Cbor.encode(ti).toByteArray)
    ).get
}

extension (utxos: Utxos) {
    def toEvacuationMap(
        network: CardanoNetwork.Section
    ): Either[Payout.Obligation.MinAdaViolation, EvacuationMap] =
        for {
            map <- utxos
                .map((ti, to) =>
                    Payout.Obligation(KeepRaw(to), network) match {
                        case Left(e)  => Left(e)
                        case Right(o) => Right(ti.toEvacuationKey, o)
                    }
                )
                .toList
                .sequence

            em = EvacuationMap(TreeMap.from(map))
        } yield em
}

extension (ek: EvacuationKey) {
    // As above: technically partial, but used in the context of the EutxoL2Ledger, it's not.
    def toTransactionInput: TransactionInput =
        Cbor.decode(ek.byteString.bytes).to[TransactionInput].value
}

extension (em: EvacuationMap) {
    def toUtxos = em.cooked.map((ti, to) => ti.toTransactionInput -> to)
}

object EutxoL2Ledger {
    type Config = CardanoNetwork.Section & InitializationParameters.Section & HeadParameters.Section

    case class State(
        activeUtxos: Utxos,
        pendingDeposits: Map[RequestId, L2Genesis],
        errors: Map[RequestId, String],
        confirmations: Map[BlockNumber, Vector[(RequestId, EnrichedTx.Serialized)]],
        headId: Option[HeadId],
        /** Monotonic commit counter — the recovery anchor (§R2b). Bumped once per successful
          * state-mutating command; the transient proxy commands (confirmations / errors) do not
          * advance it.
          */
        commandNumber: L2CommandNumber,
    )

    object State:
        /** The genesis state (commandNumber 0): `activeUtxos` from the config's initial evacuation
          * map, everything else empty. The restore base when no snapshot precedes the target
          * commandNumber.
          */
        def genesis(config: EutxoL2Ledger.Config): State =
            State(
              activeUtxos = config.initialEvacuationMap.toUtxos,
              pendingDeposits = Map.empty,
              errors = Map.empty,
              confirmations = Map.empty,
              headId = None,
              commandNumber = L2CommandNumber.zero
            )

    /** Build a ledger whose state lives only in memory (no recovery store). Convenience for callers
      * that do not need crash recovery (e.g. pure model tests).
      */
    def apply(config: EutxoL2Ledger.Config): IO[EutxoL2Ledger] =
        InMemoryL2Store.create.flatMap(store => apply(config, store))

    /** Build a ledger backed by `store` for crash recovery (§R2b): each real command appends to the
      * store's log and snapshots every [[L2Store.SnapshotInterval]] commits, so
      * [[EutxoL2Ledger.restoreTo]] can rebuild the state at any past commandNumber.
      */
    def apply(config: EutxoL2Ledger.Config, store: L2Store[IO]): IO[EutxoL2Ledger] =
        for ref <- Ref[IO].of(State.genesis(config))
        yield new EutxoL2Ledger(config, ref, store)
}

case class EutxoL2Ledger private (
    config: EutxoL2Ledger.Config,
    // Note: For now, I'm going to leave this as a `Ref`. Now that we have an `Initialize` command, it would
    // _probably_ make more sense to have this be an `Option[Ref[...]]`. But the initialize command will
    // go away in the future, so...
    private val state: Ref[IO, EutxoL2Ledger.State],
    private val store: L2Store[IO]
) extends L2Ledger[IO],
      EutxoL2LedgerReader[IO] {
    implicit def monadF: Monad[IO] = Async[IO]

    /** Apply one **real** (state-mutating) command to `s`, returning the next state with
      * `commandNumber` bumped, or an `L2LedgerError`. This is the **single deterministic
      * transition** — the live mutators call it (then derive their return value + persist), and
      * [[restoreTo]] re-folds it over the logged commands. Keeping both paths on one function is
      * what guarantees a restored state is byte-identical to the live one (§R2b "factoring"). Pure
      * given `(s, command)`: the `transit` core and the deposit folds depend only on their inputs.
      */
    private def applyMutation(
        s: EutxoL2Ledger.State,
        command: L2LedgerCommand.Real
    ): Either[L2LedgerError, EutxoL2Ledger.State] = command match
        case req: L2LedgerCommand.RegisterDeposit =>
            Try(L2Genesis.fromDepositEventRegistration(req)).toEither.left
                .map(e => L2LedgerError(s"Invalid deposit transaction payload $e"))
                .map(l2Genesis =>
                    s.focus(_.pendingDeposits)
                        .modify(_.updated(req.requestId, l2Genesis))
                        .focus(_.commandNumber)
                        .modify(_.increment)
                )

        case req: L2LedgerCommand.ApplyDepositDecisions =>
            val addedL2Utxos = req.absorbedDeposits.flatMap(id => s.pendingDeposits(id).asUtxos)
            Right(
              s.focus(_.activeUtxos)
                  .modify(_ ++ addedL2Utxos.map((i, o) => i -> o.value))
                  .focus(_.pendingDeposits)
                  .modify(_.removedAll(req.absorbedDeposits ++ req.refundedDeposits))
                  .focus(_.commandNumber)
                  .modify(_.increment)
            )

        case req: L2LedgerCommand.ApplyTransaction =>
            for
                l2Tx <- L2Tx.parse(req.l2Payload.bytes, config).left.map(L2LedgerError(_))
                newActiveUtxos <- HydrozoaTransactionMutator
                    .transit(
                      config = config,
                      time = QuantizedInstant
                          .fromPlutusPosixTime(config.slotConfig, req.blockCreationStartTime),
                      state = s.activeUtxos,
                      l2Tx = l2Tx
                    )
                    .left
                    .map(error => L2LedgerError(error.toString))
            yield s
                .focus(_.activeUtxos)
                .replace(newActiveUtxos)
                .focus(_.commandNumber)
                .modify(_.increment)

    /** Run a real command: apply the transition, persist it (append to the log; snapshot every
      * [[L2Store.SnapshotInterval]] commits), and commit the new state. Returns the new state so
      * each mutator can derive its own return value (diffs / payouts). The append + snapshot happen
      * **before** `state.set` so a crash can never leave a committed in-memory state with no
      * durable record of it (write-before-advance, §R2b).
      */
    private def commitReal(
        command: L2LedgerCommand.Real
    ): EitherT[IO, L2LedgerError, EutxoL2Ledger.State] =
        for
            s <- EitherT.right(state.get)
            next <- EitherT.fromEither[IO](applyMutation(s, command))
            _ <- EitherT.right(store.appendLog(next.commandNumber, command))
            _ <- EitherT.right(
              IO.whenA(next.commandNumber.value % L2Store.SnapshotInterval == 0L)(
                store.putSnapshot(next.commandNumber, L2Snapshot.fromState(next))
              )
            )
            _ <- EitherT.right(state.set(next))
        yield next

    override def sendProxyBlockConfirmation(
        req: L2LedgerCommand.ProxyBlockConfirmation
    ): EitherT[IO, L2LedgerError, Unit] =
        EitherT.right(
          state.update(
            _.focus(_.confirmations)
                .modify(c => c.updated(req.blockNumber, req.refundTxs))
          )
        )

    override def sendProxyHydrozoaRequestError(
        req: L2LedgerCommand.ProxyRequestError
    ): EitherT[IO, L2LedgerError, Unit] =
        EitherT.right(
          state.update(
            _.focus(_.errors)
                .modify(c => c.updated(req.requestId, req.message))
          )
        )

    override def sendApplyTransaction(
        req: L2LedgerCommand.ApplyTransaction
    ): EitherT[IO, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])] =
        for {
            before <- EitherT.right(state.get)
            l2Tx <- EitherT.fromEither(
              L2Tx.parse(req.l2Payload.bytes, config).left.map(error => L2LedgerError(error))
            )
            after <- commitReal(req)
            // Diffs are the symmetric difference between the pre- and post-commit utxo sets.
            adds <-
                EitherT.fromEither(
                  after.activeUtxos
                      .removedAll(before.activeUtxos.keys)
                      .map((ti, to) =>
                          Payout
                              .Obligation(KeepRaw(to), config)
                              .map(obligation =>
                                  EvacuationDiff.Update(ti.toEvacuationKey, obligation)
                              )
                      )
                      .toVector
                      .sequence
                      .left
                      .map(error => L2LedgerError(error.toString))
                )
            deletes =
                before.activeUtxos
                    .removedAll(after.activeUtxos.keys)
                    .map((ti, _) => EvacuationDiff.Delete(ti.toEvacuationKey))
                    .toVector
            obligations <- EitherT.fromEither(
              l2Tx.l1utxos
                  .traverse((utxo: (TransactionInput, TransactionOutput)) =>
                      Payout.Obligation(KeepRaw(utxo._2), config)
                  )
                  .left
                  .map(error => L2LedgerError(error.toString))
            )
        } yield (adds ++ deletes, Vector.from(obligations))

    override def sendRegisterDeposit(
        req: RegisterDeposit
    ): EitherT[IO, L2LedgerError, Unit] =
        commitReal(req).void

    override def currentCommandNumber: IO[L2CommandNumber] = state.get.map(_.commandNumber)

    /** The committed L2 utxos this address controls — a live filter over `activeUtxos`. Concurrent
      * with the JointLedger-driven command path (a plain `Ref` read), so it observes the state as
      * of the last committed command.
      */
    override def utxosByAddress(address: Address): IO[Utxos] =
        state.get.map(_.activeUtxos.filter((_, output) => output.address == address))

    /** The most recent `limit` applied L2 transactions, newest first, projected to
      * [[L2TxSummary]]s. Reads the tail of the store's own command log, so it needs no separate
      * history: for the EUTXO ledger the logged command *is* the record of what happened.
      *
      * `limit` counts returned summaries, not commands scanned — one command can expand to several
      * summaries and a no-op deposit decision to none — so the log window is widened backward until
      * `limit` summaries are collected or the log is exhausted. Each earlier batch is strictly
      * older, so accumulating preserves newest-first order.
      */
    override def recentTransactions(limit: Int): IO[Vector[L2TxSummary]] =
        if limit <= 0 then IO.pure(Vector.empty)
        else
            state.get.map(_.commandNumber).flatMap { current =>
                def collect(
                    upTo: L2CommandNumber,
                    acc: Vector[L2TxSummary]
                ): IO[Vector[L2TxSummary]] =
                    if acc.sizeIs >= limit || upTo.value <= 0L then IO.pure(acc)
                    else
                        val fromExclusive =
                            L2CommandNumber(math.max(0L, upTo.value - limit.toLong))
                        store
                            .logRange(fromExclusive, upTo)
                            .flatMap(commands =>
                                collect(
                                  fromExclusive,
                                  acc ++ commands.reverse.flatMap(L2TxSummary.fromCommand)
                                )
                            )
                collect(current, Vector.empty).map(_.take(limit))
            }

    /** Read the full in-memory state — package-private, for recovery tests that assert a restored
      * ledger matches the live one. Not part of the [[L2Ledger]] interface.
      */
    private[eutxol2] def peekState: IO[EutxoL2Ledger.State] = state.get

    /** Reconstruct the committed state as of `commandNumber` (§R2b): load the latest snapshot
      * `<= commandNumber` (or genesis), then re-fold the logged commands in
      * `(snapshot.commandNumber, commandNumber]` through the same [[applyMutation]] the live path
      * uses — no re-logging, no re-snapshot, no commandNumber re-bump. Replaces the in-memory
      * state; afterwards [[currentCommandNumber]] equals `commandNumber`.
      */
    override def restoreTo(commandNumber: L2CommandNumber): EitherT[IO, L2LedgerError, Unit] =
        for {
            base <- EitherT.right(
              store
                  .latestSnapshotAtOrBefore(commandNumber)
                  .map(_.fold(EutxoL2Ledger.State.genesis(config))(restoreFromSnapshot))
            )
            commands <- EitherT.right(store.logRange(base.commandNumber, commandNumber))
            restored <- EitherT.fromEither[IO](
              commands.foldLeft[Either[L2LedgerError, EutxoL2Ledger.State]](Right(base))(
                (acc, cmd) => acc.flatMap(applyMutation(_, cmd))
              )
            )
            _ <- EitherT.cond[IO](
              restored.commandNumber == commandNumber,
              (),
              L2LedgerError(
                s"restoreTo($commandNumber) reached commandNumber ${restored.commandNumber}: log is missing entries"
              )
            )
            _ <- EitherT.right(state.set(restored))
        } yield ()

    /** Rebuild a full [[EutxoL2Ledger.State]] from a persisted snapshot — `activeUtxos`,
      * `pendingDeposits`, and `commandNumber` come from the snapshot; the transient `errors` /
      * `confirmations` are not persisted and start empty (§R2b).
      */
    private def restoreFromSnapshot(entry: (L2CommandNumber, L2Snapshot)): EutxoL2Ledger.State =
        val snapshot = entry._2
        EutxoL2Ledger.State
            .genesis(config)
            .copy(
              activeUtxos = snapshot.activeUtxos,
              pendingDeposits = snapshot.pendingDeposits,
              commandNumber = snapshot.commandNumber
            )

    override def sendApplyDepositDecisions(
        req: L2LedgerCommand.ApplyDepositDecisions
    ): EitherT[IO, L2LedgerError, Vector[EvacuationDiff]] =
        for {
            before <- EitherT.right(state.get)
            // Diffs are derived from the pre-commit pending deposits (the ones being absorbed).
            addedL2Utxos = req.absorbedDeposits.flatMap(id => before.pendingDeposits(id).asUtxos)
            _ <- commitReal(req)
            evacuationDiffs <-
                EitherT.fromEither(
                  Vector
                      .from(
                        addedL2Utxos.map((i, o) =>
                            Payout
                                .Obligation(o, config)
                                .map(obligation =>
                                    EvacuationDiff.Update(i.toEvacuationKey, obligation)
                                )
                        )
                      )
                      .sequence
                      .left
                      .map(e => L2LedgerError(e.toString))
                )
        } yield evacuationDiffs
}

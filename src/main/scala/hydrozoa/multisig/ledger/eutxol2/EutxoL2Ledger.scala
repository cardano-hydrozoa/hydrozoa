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
import hydrozoa.multisig.ledger.eutxol2.store.{L2Snapshot, L2Store}
import hydrozoa.multisig.ledger.eutxol2.tx.{L2Genesis, L2Tx}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey, EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l2.*
import hydrozoa.multisig.ledger.l2.L2CommandNumber.increment
import hydrozoa.multisig.ledger.l2.L2LedgerCommand.RegisterDeposit
import hydrozoa.multisig.ledger.l2.L2LedgerResponse.DepositDecisionRejectReason
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
        headId: Option[HeadId],
        /** The command number of the last command evaluated — applied *or* rejected (the
          * coordination index / recovery tip). JointLedger assigns it; the ledger validates and
          * adopts it (it does not self-increment). A trailing rejection advances it past the last
          * logged command, so it is not derivable from the log alone.
          */
        commandNumber: L2CommandNumber,
        /** The command number of the `ApplyDepositDecisions` that froze the ledger, or `None` when
          * not frozen. A frozen ledger refuses every command (fail-stop) until [[restoreTo]]
          * rewinds to before the freeze; persisted via `store.putFrozenAt` so it survives a crash.
          */
        frozenAt: Option[L2CommandNumber],
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
              headId = None,
              commandNumber = L2CommandNumber.zero,
              frozenAt = None
            )

    /** Build a ledger backed by `store` for crash recovery: each *applied* command appends to the
      * store's (sparse) log, and every [[L2Store.SnapshotInterval]]-th command number snapshots; a
      * rejected command advances the tip only. [[EutxoL2Ledger.restoreTo]] rebuilds the state at
      * any past commandNumber.
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
      L2Screener[IO],
      EutxoL2LedgerReader[IO] {
    implicit def monadF: Monad[IO] = Async[IO]

    /** Apply one state-mutating command to `s`, returning the next state with `commandNumber` set
      * to the caller-assigned value, or an error message. This is the **single deterministic
      * transition** — the live path ([[submit]], which validates the number first) calls it, and
      * [[restoreTo]] re-folds it over the logged commands. Keeping both paths on one function is
      * what guarantees a restored state is byte-identical to the live one (§R2b "factoring"). Pure
      * given `(commandNumber, s, command)`: the `transit` core and the deposit folds depend only on
      * their inputs.
      */
    private def applyMutation(
        commandNumber: L2CommandNumber,
        s: EutxoL2Ledger.State,
        command: L2LedgerCommand
    ): Either[String, EutxoL2Ledger.State] = command match
        case req: L2LedgerCommand.RegisterDeposit =>
            // No validity gate here — this transition is re-folded by `restoreTo`, and replay must
            // reconstruct state, not re-litigate validity. The gate lives on the live path, in
            // `registerDeposit` (see [[validateSpawnedOutputs]]).
            Try(L2Genesis.fromDepositEventRegistration(req)).toEither.left
                .map(e => s"Invalid deposit transaction payload $e")
                .map(l2Genesis =>
                    s.focus(_.pendingDeposits)
                        .modify(_.updated(req.requestId, l2Genesis))
                        .focus(_.commandNumber)
                        .replace(commandNumber)
                )

        case req: L2LedgerCommand.ApplyDepositDecisions =>
            val addedL2Utxos = req.absorbedDeposits.flatMap(id => s.pendingDeposits(id).asUtxos)
            Right(
              s.focus(_.activeUtxos)
                  .modify(_ ++ addedL2Utxos.map((i, o) => i -> o.value))
                  .focus(_.pendingDeposits)
                  .modify(_.removedAll(req.absorbedDeposits ++ req.rejectedDeposits))
                  .focus(_.commandNumber)
                  .replace(commandNumber)
            )

        case req: L2LedgerCommand.ApplyTransaction =>
            for
                l2Tx <- L2Tx.parse(req.l2Payload.bytes, config)
                newActiveUtxos <- HydrozoaTransactionMutator
                    .transit(
                      config = config,
                      time = QuantizedInstant
                          .fromPlutusPosixTime(config.slotConfig, req.blockCreationStartTime),
                      state = s.activeUtxos,
                      l2Tx = l2Tx
                    )
                    .left
                    .map(error => error.toString)
            yield s
                .focus(_.activeUtxos)
                .replace(newActiveUtxos)
                .focus(_.commandNumber)
                .replace(commandNumber)

    /** The registration-time validity gate for a deposit's spawned L2 outputs: each must clear
      * min-ada on its own — the same [[Payout.Obligation]] check absorption applies. Enforced on
      * the live path in [[registerDeposit]] (authoritative) and [[screenDeposit]] (early), so a
      * deposit that registers is guaranteed to absorb: a sub-min-ada output would otherwise fail
      * only at absorption ([[applyDepositDecisions]]), which cannot reject, and wedge the block on
      * every recovery re-drive. Deliberately *not* inside [[applyMutation]]: that is the single
      * deterministic transition [[restoreTo]] re-folds over logged commands, and replay must
      * reconstruct state, not re-litigate validity — re-validating a deposit an older binary logged
      * without this gate (or under different min-ada params) would abort recovery. (Aggregate cover
      * is the sibling [[validateDepositCover]] gate.)
      */
    private def validateSpawnedOutputs(l2Genesis: L2Genesis): Either[String, Unit] =
        l2Genesis.asUtxos.values.toVector
            .traverse(output => Payout.Obligation(output, config))
            .map(_ => ())
            .leftMap(e => s"deposit spawns an invalid L2 output: $e")

    /** The value-conservation gate: a deposit's spawned L2 outputs must be covered by
      * `depositL2Value` (the L1 value the treasury absorbs), so a deposit cannot mint L2 value —
      * covered means the difference is non-negative in the coin and in every asset. Enforced on the
      * live path in [[registerDeposit]] (authoritative — every peer, and the only enforcement for a
      * remote-backed node whose [[screenDeposit]] is a stub) and [[screenDeposit]] (early); like
      * [[validateSpawnedOutputs]] it is kept off [[applyMutation]], so [[restoreTo]] stays a pure
      * reconstruction.
      */
    private def validateDepositCover(
        l2Genesis: L2Genesis,
        depositL2Value: Value
    ): Either[String, Unit] =
        val spawnedValue = Value.combine(l2Genesis.genesisObligations.map(_.l2OutputValue))
        val diff = depositL2Value - spawnedValue
        Either.cond(
          diff.coin.value >= 0 && diff.assets.assets.values.forall(_.values.forall(_ >= 0)),
          (),
          s"deposit l2Payload outputs ($spawnedValue) exceed depositL2Value ($depositL2Value)"
        )

    /** The gate shared by all three command methods: fail-stop unless `commandNumber` is the
      * ledger's next expected number. A **frozen** ledger yields a
      * [[L2LedgerResponse.LedgerFreeze]] to answer; an out-of-order number yields a
      * [[L2LedgerResponse.OutOfOrder]] (a desync, not a per-request verdict — JointLedger
      * fail-stops on it). Otherwise it yields the current state to evaluate the command against.
      * The command number advances on **every** evaluated command (it is a coordination index, not
      * ledger state): a fresh command that applies is persisted (log + tip + state); a fresh
      * command the ledger rejects advances the tip alone. Unreachable while JointLedger drives
      * strictly single-in-flight and in lock-step, but keeps the invariant self-checking if that
      * ever breaks.
      */
    private def freshOrFail(
        commandNumber: L2CommandNumber
    ): IO[Either[
      L2LedgerResponse.OutOfOrder | L2LedgerResponse.LedgerFreeze,
      EutxoL2Ledger.State
    ]] =
        state.get.map { before =>
            before.frozenAt match
                case Some(frozen) => Left(L2LedgerResponse.LedgerFreeze(commandNumber, frozen))
                case None =>
                    val expected = before.commandNumber.increment
                    if commandNumber != expected then
                        Left(L2LedgerResponse.OutOfOrder(commandNumber, expected))
                    else Right(before)
        }

    /** Advance the tip for a *rejected* fresh command — durably and in memory — without logging or
      * changing state, so the coordination index stays in lock-step with JointLedger's. Snapshots
      * on an interval boundary just like [[persist]] (the state is unchanged, but the snapshot
      * anchors the boundary), so a reject landing on a boundary can't leave a permanent gap in the
      * snapshot cadence.
      */
    private def rejectAndAdvance(commandNumber: L2CommandNumber): IO[Unit] =
        store.putTip(commandNumber) >>
            state.get.flatMap { s =>
                val advanced = s.focus(_.commandNumber).replace(commandNumber)
                IO.whenA(commandNumber.value % L2Store.SnapshotInterval == 0L)(
                  store.putSnapshot(commandNumber, L2Snapshot.fromState(advanced))
                ) >> state.set(advanced)
            }

    /** Reject a decision the ledger could not apply and **freeze**: advance the tip (the command
      * was evaluated, like any reject) and record the freeze — in the store and in memory — so
      * every subsequent command fail-stops until [[restoreTo]] rewinds to before `commandNumber`.
      */
    private def rejectAndFreeze(commandNumber: L2CommandNumber): IO[Unit] =
        rejectAndAdvance(commandNumber) >>
            store.putFrozenAt(Some(commandNumber)) >>
            state.update(_.focus(_.frozenAt).replace(Some(commandNumber)))

    /** Durably advance to `next`: append the command to the log, snapshot every
      * [[L2Store.SnapshotInterval]] commits, record the tip, then set the in-memory state — in that
      * order, so a crash can never leave a committed in-memory state with no durable record of it
      * (write-before-advance). The ledger's only apply-advance, run **last** on a successful
      * command.
      */
    private def persist(next: EutxoL2Ledger.State, command: L2LedgerCommand): IO[Unit] =
        store.appendLog(next.commandNumber, command) >>
            IO.whenA(next.commandNumber.value % L2Store.SnapshotInterval == 0L)(
              store.putSnapshot(next.commandNumber, L2Snapshot.fromState(next))
            ) >>
            store.putTip(next.commandNumber) >>
            state.set(next)

    override def screenTx(l2Payload: ByteString): EitherT[IO, L2ScreenError, Unit] =
        // The native L2 tx must parse, carry this head's headId pin, and have valid vkey-witness
        // signatures over the tx id. All stateless; the stateful required-signers / balance / input
        // checks stay at submission (an unsigned tx that slips past here is still rejected there by
        // MissingKeyHashes).
        EitherT.fromEither[IO](for {
            l2Tx <- L2Tx.parse(l2Payload.bytes, config).left.map(L2ScreenError(_))
            _ <- HeadIdPinValidator.validate(config, l2Tx.tx).left.map(L2ScreenError(_))
            _ <- HydrozoaTransactionMutator
                .screenSignatures(config, l2Tx)
                .left
                .map(e => L2ScreenError(e.toString))
        } yield ())

    override def screenDeposit(
        req: L2LedgerCommand.ScreenDeposit
    ): EitherT[IO, L2ScreenError, Unit] =
        EitherT.fromEither[IO](for {
            // The l2Payload must decode to the deposit's GenesisObligations — the utxos this ledger
            // will spawn when the deposit is absorbed. Shares the decode with registration
            // (fromDepositPayload), so nothing screened here can fail to register or absorb later.
            l2Genesis <- Try(
              L2Genesis.fromDepositPayload(req.depositId, req.l2Payload)
            ).toEither.left.map(e => L2ScreenError(s"Invalid deposit transaction payload $e"))
            // depositL2Value must cover the spawned outputs, and each output must clear min-ada on
            // its own — the same two gates registration applies, so a screened deposit can be both
            // registered and absorbed.
            _ <- validateDepositCover(l2Genesis, req.depositL2Value).left.map(L2ScreenError(_))
            _ <- validateSpawnedOutputs(l2Genesis).left.map(L2ScreenError(_))
        } yield ())

    override def applyTransaction(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyTransaction
    ): IO[ApplyTransactionResponse] =
        freshOrFail(commandNumber).flatMap {
            case Left(failure) => IO.pure(failure)
            case Right(before) =>
                val attempt = for {
                    l2Tx <- L2Tx.parse(req.l2Payload.bytes, config)
                    next <- applyMutation(commandNumber, before, req)
                    // Diffs are the symmetric difference between the pre- and post-apply utxo sets.
                    adds <- next.activeUtxos
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
                        .map(error => error.toString)
                    deletes = before.activeUtxos
                        .removedAll(next.activeUtxos.keys)
                        .map((ti, _) => EvacuationDiff.Delete(ti.toEvacuationKey))
                        .toVector
                    obligations <- l2Tx.l1utxos
                        .traverse((utxo: (TransactionInput, TransactionOutput)) =>
                            Payout.Obligation(KeepRaw(utxo._2), config)
                        )
                        .left
                        .map(error => error.toString)
                } yield (
                  next,
                  L2LedgerResponse.Applied.ApplyTransaction(
                    commandNumber,
                    adds ++ deletes,
                    Vector.from(obligations)
                  )
                )
                attempt match
                    case Right((next, applied)) => persist(next, req).as(applied)
                    case Left(reject) =>
                        rejectAndAdvance(commandNumber)
                            .as(L2LedgerResponse.Rejected.ApplyTransaction(commandNumber, reject))
        }

    override def registerDeposit(
        commandNumber: L2CommandNumber,
        req: RegisterDeposit
    ): IO[RegisterDepositResponse] =
        freshOrFail(commandNumber).flatMap {
            case Left(failure) => IO.pure(failure)
            case Right(before) =>
                val attempt = for {
                    l2Genesis <- Try(L2Genesis.fromDepositEventRegistration(req)).toEither.left
                        .map(e => s"Invalid deposit transaction payload $e")
                    // Reject a deposit that fails value conservation (cover) or spawns a sub-min-ada
                    // output on the live path, before applyMutation — deliberately not inside
                    // applyMutation, so restoreTo's replay never re-litigates validity.
                    _ <- validateDepositCover(l2Genesis, req.depositL2Value)
                    _ <- validateSpawnedOutputs(l2Genesis)
                    next <- applyMutation(commandNumber, before, req)
                } yield next
                attempt match
                    case Right(next) =>
                        persist(next, req).as(
                          L2LedgerResponse.Applied.RegisterDeposit(commandNumber)
                        )
                    case Left(reject) =>
                        rejectAndAdvance(commandNumber)
                            .as(L2LedgerResponse.Rejected.RegisterDeposit(commandNumber, reject))
        }

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

    /** Read the full in-memory state — for recovery tests that assert a restored ledger matches the
      * live one (and observe the command number). Not part of the [[L2Ledger]] interface.
      */
    private[multisig] def peekState: IO[EutxoL2Ledger.State] = state.get

    /** Reconstruct the committed state as of `commandNumber`: load the latest snapshot
      * `<= commandNumber` (or genesis), then re-fold the *logged* (applied) commands in
      * `(snapshot.commandNumber, commandNumber]` through the same [[applyMutation]] the live path
      * uses — no re-logging, no re-snapshot. The command number is a coordination index with gaps
      * where commands were rejected, so `commandNumber` may land on a gap; the applied subset still
      * yields the state at that point, and the target is adopted as the tip. Guarded against a
      * target beyond the recorded tip (a corruption tripwire — the co-anchoring ordering prevents
      * it).
      */
    override def restoreTo(commandNumber: L2CommandNumber): EitherT[IO, RestoreError, Unit] =
        for {
            tip <- EitherT.right(store.getTip.map(_.getOrElse(L2CommandNumber.zero)))
            _ <- EitherT.cond[IO](
              Ordering[L2CommandNumber].lteq(commandNumber, tip),
              (),
              RestoreError.CommandNumberTooHigh(commandNumber, tip)
            )
            base <- EitherT.right(
              store
                  .latestSnapshotAtOrBefore(commandNumber)
                  .map(_.fold(EutxoL2Ledger.State.genesis(config))(restoreFromSnapshot))
            )
            commands <- EitherT.right(store.logRange(base.commandNumber, commandNumber))
            restored <- EitherT.fromEither[IO](
              commands
                  .foldLeft[Either[String, EutxoL2Ledger.State]](Right(base))((acc, cmd) =>
                      acc.flatMap(s => applyMutation(s.commandNumber.increment, s, cmd))
                  )
                  .left
                  .map(RestoreError.InternalLedgerError(_))
            )
            // Respect the freeze: it survives only if it happened at or before the target — rewinding
            // to before the freezing decision clears it.
            frozenAt <- EitherT.right(
              store.getFrozenAt.map(_.filter(Ordering[L2CommandNumber].lteq(_, commandNumber)))
            )
            _ <- EitherT.right(store.putTip(commandNumber))
            _ <- EitherT.right(store.putFrozenAt(frozenAt))
            _ <- EitherT.right(
              state.set(
                restored
                    .focus(_.commandNumber)
                    .replace(commandNumber)
                    .focus(_.frozenAt)
                    .replace(frozenAt)
              )
            )
        } yield ()

    /** Rebuild a full [[EutxoL2Ledger.State]] from a persisted snapshot — `activeUtxos`,
      * `pendingDeposits`, and `commandNumber` come from the snapshot (§R2b).
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

    override def applyDepositDecisions(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyDepositDecisions
    ): IO[ApplyDepositDecisionsResponse] =
        freshOrFail(commandNumber).flatMap {
            case Left(failure) => IO.pure(failure)
            case Right(before) =>
                val attempt = for {
                    // Unknown deposit compartment: the decision names a deposit the ledger never
                    // registered (a JointLedger bug, not a deposit-validity failure) — reject + freeze.
                    absorbed <- req.absorbedDeposits.traverse(id =>
                        before.pendingDeposits
                            .get(id)
                            .toRight(DepositDecisionRejectReason.CompartmentNotFound(id))
                    )
                    next <- applyMutation(commandNumber, before, req).left
                        .map(DepositDecisionRejectReason.InternalLedgerError(_))
                    // Registration validated each output's min-ada, so this should not fail; if it
                    // does it is an internal merge error (a ledger bug) — reject + freeze.
                    diffs <- absorbed
                        .flatMap(_.asUtxos)
                        .toVector
                        .traverse((i, o) =>
                            Payout
                                .Obligation(o, config)
                                .map(obligation =>
                                    EvacuationDiff.Update(i.toEvacuationKey, obligation)
                                )
                        )
                        .leftMap(e =>
                            DepositDecisionRejectReason.InternalLedgerError(
                              s"internal merge error: $e"
                            )
                        )
                } yield (next, L2LedgerResponse.Applied.ApplyDepositDecisions(commandNumber, diffs))
                // A rejected decision is a coordination bug, so instead of advancing the tip alone it
                // **freezes** the ledger: every later command then answers LedgerFreeze until
                // restoreTo rewinds past it. JointLedger panics on the returned Rejected.
                attempt match
                    case Right((next, applied)) => persist(next, req).as(applied)
                    case Left(reject) =>
                        rejectAndFreeze(commandNumber)
                            .as(
                              L2LedgerResponse.Rejected.ApplyDepositDecisions(commandNumber, reject)
                            )
        }
}

package hydrozoa.multisig.consensus

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.consensus.transport.CoilTransport
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.{AdoptedStartPoint, JournalKey, JournalValue, Markers, Persistence, StoreKey, WriteBatch}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** The coil peer's half of the join exchange: adopting the start point its hub offered.
  *
  * **This runs before the node's actors exist, and it can only run there.** `L2Ledger.importState`
  * accepts a state only into a ledger that has applied nothing, and by the time any actor is
  * listening `JointLedger` and `StackComposer` have already positioned themselves off this store.
  * So a start point is adopted at boot or not at all — [[hydrozoa.multisig.consensus.liaison]]'s
  * coil liaison declines one that arrives later.
  *
  * What it leaves behind is an ordinary store that the ordinary recovery paths read: the treasury
  * and evacuation map the slow side opens from, the block brief, deposit map and command number the
  * fast side resumes from, and one [[AdoptedStartPoint]] recording the anchors this peer has no own
  * production to derive.
  */
object CoilJoin {

    type Config = JoinOfferVerifier.Config & CardanoNetwork.Section & OwnPeerPublic.Section

    /** Check an offer and, if it holds up, seed the store from it.
      *
      * **Nothing is trusted because it arrived.** The digests this checks are the ones the coil's
      * own ledger reports after adopting, never the ones travelling with the bytes — a hash beside
      * the thing it describes attests to nothing.
      *
      * **Adopting destroys what was there.** A peer being seeded holds nothing worth keeping: its
      * ledger is too far behind for its hub to serve it forward, and its stale journals would
      * anchor recovery below the start point on history the hub no longer has. So both stores are
      * wiped rather than merged into.
      *
      * The order is what makes that safe. Everything checkable without the ledger — the settlement
      * is a transaction this head could have produced, it belongs to this head, the signatures hold
      * — is checked **before** anything is destroyed, so a forged offer costs the coil nothing. An
      * offer that clears those and then fails on digests took N-of-N head signatures to build.
      *
      * A crash anywhere after the wipe leaves a cold store, which rejoins cleanly on the next boot.
      */
    def adopt(
        offer: Join.Offer,
        persistence: Persistence[IO],
        ledger: L2Ledger[IO]
    )(using config: Config): IO[Unit] =
        for {
            certificate <- JoinOfferVerifier.verifyCertificate(offer.settlement, offer.sec)
            _ <- IO.fromEither(certificate)
            _ <- ledger.wipe.value.flatMap(IO.fromEither)
            _ <- persistence.backend.wipeData
            digests <- ledger.importState(offer.state).value.flatMap(IO.fromEither)
            _ <- IO.fromEither(
              JoinOfferVerifier.verifyAdoptedState(offer.settlement, offer.sec, digests)
            )
            map <- ledger.evacuationMapAt(offer.state.commandNumber).value.flatMap(IO.fromEither)
            stamp <- persistence.arrivalStamp
            lastBlockNum = offer.block.blockNum
            _ <- persistence.write(
              WriteBatch.start
                  .put(StoreKey.StartPoint)(
                    AdoptedStartPoint(
                      startStack = offer.startStack,
                      lastBlockNum = lastBlockNum,
                      commandNumber = offer.state.commandNumber,
                      ownHardAckStart = offer.ownHardAck,
                      cursors = offer.cursors
                    )
                  )
                  .put(StoreKey.Treasury)(offer.settlement.treasuryProduced)
                  .put(StoreKey.EvacuationMap(lastBlockNum))(map)
                  // Everything the boot path reads AT THE ANCHOR BLOCK. The coil pulls from
                  // `lastBlockNum + 1`, so this is the one block it is never served and the one
                  // block whose rows it cannot obtain any other way. `JointLedger.doneAt` wants
                  // the first three; `ReplayActor` wants the fourth as its request-lane floor.
                  .put(JournalKey.Block(lastBlockNum))(JournalValue(stamp, offer.block))
                  .put(StoreKey.DepositMap(lastBlockNum))(offer.deposits)
                  .put(StoreKey.L2CommandNumber(lastBlockNum))(offer.state.commandNumber)
                  // The hub read this key to build `cursors.requests`, so writing the cursors back
                  // reconstitutes it exactly.
                  .put(StoreKey.RequestHighWater(lastBlockNum))(offer.cursors.requests)
            )
        } yield ()

    /** The start point this peer was seeded at, or `None` for a peer that produced its own history
      * — every head peer, and any coil peer that bootstrapped stack 0.
      */
    def adoptedStartPoint(persistence: Persistence[IO]): IO[Option[AdoptedStartPoint]] =
        persistence.get(StoreKey.StartPoint)

    /** Settle where this coil peer starts, before any of its actors exist. Returns once the store
      * is the one the node should boot from.
      *
      * **A cold store waits for its hub; a warm one does not.** Booting cold without an answer is
      * the failure this ticket exists to fix: the coil re-derives stack 0 from config, the head is
      * long past it, and nothing afterwards can reconcile the two — the node looks healthy and is
      * permanently useless. Waiting is the honest alternative, and it clears the moment the hub
      * answers. A coil with history has somewhere to walk forward from, so it proceeds on its own
      * after [[warmJoinWait]] rather than blocking a working node on an unreachable hub.
      *
      * `NoOffer` is an answer, not a timeout: at a real bring-up every coil is cold and every hub
      * says there is nothing to seed from, and all of them boot straight through this.
      *
      * **An offer is adopted whether the store is cold or warm**, and adopting discards whatever
      * was there. A stale coil is the case the whole exchange is for — the empty store is its
      * degenerate form — and a hub only offers when the coil is too far behind to be walked
      * forward, so there is nothing left to preserve. See [[adopt]].
      */
    def settleStartPoint(
        transport: CoilTransport,
        persistence: Persistence[IO],
        ledger: L2Ledger[IO],
        tracer: ContraTracer[IO, CoilJoinEvent]
    )(using config: Config): IO[Unit] =
        for {
            markers <- Markers.derive(persistence, config.ownPeerId)
            startPoint <- adoptedStartPoint(persistence)
            // Where this coil stands, for the hub to decide on. A dialing transport already sent
            // this in its handshake and ignores the call.
            _ <- transport.announceMarks(
              Join.Connected(block = markers.fastBlockMark, stack = markers.hardConfirmed)
            )
            cold = markers.hardAckedStack.isEmpty && startPoint.isEmpty
            answer <-
                if cold then waitForAnswer(transport, tracer).map(Some(_))
                else transport.joinAnswer.map(Some(_)).timeoutTo(warmJoinWait, IO.none)
            _ <- answer match {
                case Some(offer: Join.Offer) =>
                    tracer.traceWith(CoilJoinEvent.Adopting(offer.startStack)) >>
                        adopt(offer, persistence, ledger) >>
                        tracer.traceWith(
                          CoilJoinEvent
                              .Adopted(offer.startStack, offer.block.blockNum, offer.cursors.block)
                        )
                case Some(no: Join.NoOffer) =>
                    tracer.traceWith(CoilJoinEvent.NothingToAdopt(no.reason))
                case None =>
                    tracer.traceWith(CoilJoinEvent.HubSilent(warmJoinWait))
            }
        } yield ()

    /** How long a coil peer that already has history waits for its hub before booting anyway. Not
      * configurable: past it the node boots and catches up, so the only cost of the wait being
      * wrong is a slower start or a missed seeding that the next reconnect offers again.
      */
    val warmJoinWait: FiniteDuration = 30.seconds

    /** Wait as long as it takes, saying so periodically — a cold coil has nothing useful to do
      * without an answer, and the log line is what tells an operator the hub is the problem.
      */
    private def waitForAnswer(
        transport: CoilTransport,
        tracer: ContraTracer[IO, CoilJoinEvent]
    ): IO[Join.Answer] =
        transport.joinAnswer
            .race(
              (IO.sleep(coldJoinReportEvery) >> tracer.traceWith(
                CoilJoinEvent.StillWaiting
              )).foreverM
            )
            .map(_.merge)

    private val coldJoinReportEvery: FiniteDuration = 10.seconds
}

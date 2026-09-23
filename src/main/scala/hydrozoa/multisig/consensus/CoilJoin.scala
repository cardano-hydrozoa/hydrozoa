package hydrozoa.multisig.consensus

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.{AdoptedStartPoint, JournalKey, JournalValue, Markers, Persistence, StoreKey, WriteBatch}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** The coil peer's half of the join exchange: adopting the start point its hub offered.
  *
  * **This runs while the coil liaison is in join mode, before any other actor has read the store.**
  * `L2Ledger.importState` accepts a state only into a ledger that has applied nothing, so adoption
  * has to precede `JointLedger` and `StackComposer` positioning themselves. They do that behind the
  * regime manager's connections barrier, which is what leaves the window open: the coil liaison is
  * spawned and joins first, and the barrier opens only once it has become the regular liaison. An
  * offer reaching the liaison after that is a late redial and is declined.
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
      * own ledger computes from the offered bytes, never the ones travelling with them — a hash
      * beside the thing it describes attests to nothing.
      *
      * **The whole offer is checked before anything of the coil's is destroyed.** Verifying the
      * certificate establishes only that the head peers signed a settlement; a settlement is an
      * ordinary L1 transaction, so anyone can pair a genuine one with arbitrary bytes, and the
      * digest comparison is the only thing that joins the two. Running it after the wipe would mean
      * a single wrong hub — malicious, or merely exporting the wrong boundary — could cost a coil
      * its store on every join it refuses. So the ledger digests the blob where it lies, and the
      * coil commits to nothing until both halves pass (GUM-354).
      *
      * **Adopting destroys what was there.** A peer being seeded holds nothing worth keeping: its
      * ledger is too far behind for its hub to serve it forward, and its stale journals would
      * anchor recovery below the start point on history the hub no longer has. So both stores are
      * wiped rather than merged into.
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
            digests <- ledger.digestsOf(offer.state).value.flatMap(IO.fromEither)
            map <- ledger.evacuationMapOf(offer.state).value.flatMap(IO.fromEither)
            _ <- IO.fromEither(
              JoinOfferVerifier.verifyOfferedState(offer.settlement, offer.sec, digests, map)
            )
            _ <- ledger.wipe.value.flatMap(IO.fromEither)
            _ <- persistence.backend.wipeData
            _ <- ledger.importState(offer.state).value.flatMap(IO.fromEither)
            stamp <- persistence.arrivalStamp
            lastBlockNum = offer.block.blockNum
            startPoint = AdoptedStartPoint(
              startStack = offer.startStack,
              lastBlockNum = lastBlockNum,
              commandNumber = offer.state.commandNumber,
              ownHardAckStart = offer.ownHardAck,
              cursors = offer.cursors
            )
            _ <- persistence.write(
              WriteBatch.start
                  .put(StoreKey.StartPoint)(startPoint)
                  .put(StoreKey.Treasury)(offer.settlement.treasuryProduced)
                  .put(StoreKey.EvacuationMap(lastBlockNum))(map)
                  // Everything the boot path reads AT THE ANCHOR BLOCK. The coil pulls from
                  // `lastBlockNum + 1`, so this is the one block it is never served and the one
                  // block whose rows it cannot obtain any other way. `JointLedger.doneAt` wants
                  // the first three; `ReplayActor` wants the fourth as its request-lane floor.
                  .put(JournalKey.Block(lastBlockNum))(JournalValue(stamp, offer.block))
                  .put(StoreKey.DepositMap(lastBlockNum))(offer.deposits)
                  .put(StoreKey.L2CommandNumber(lastBlockNum))(offer.state.commandNumber)
                  .put(StoreKey.RequestHighWater(lastBlockNum))(startPoint.requestHighWater)
            )
        } yield ()

    /** The start point this peer was seeded at, or `None` for a peer that produced its own history
      * — every head peer, and any coil peer that bootstrapped stack 0.
      */
    def adoptedStartPoint(persistence: Persistence[IO]): IO[Option[AdoptedStartPoint]] =
        persistence.get(StoreKey.StartPoint)

    /** Where this coil stands, for its hub to decide on, and how long it may wait for the answer.
      *
      * Read once by the coil liaison as it enters join mode. The marks go out to the hub — a
      * dialing transport already sent them in its handshake and ignores the call — and the
      * [[JoinWait]] arms the liaison's join-mode timer.
      *
      * **A cold store waits for its hub; a warm one does not.** Booting cold without an answer is
      * the failure the exchange exists to prevent: the coil re-derives stack 0 from config, the
      * head is long past it, and nothing afterwards can reconcile the two — the node looks healthy
      * and is permanently useless. Waiting is the honest alternative, and it clears the moment the
      * hub answers. A coil with history has somewhere to walk forward from, so it proceeds on its
      * own after [[warmJoinWait]] rather than blocking a working node on an unreachable hub.
      *
      * `NoOffer` is an answer, not a timeout: at a real bring-up every coil is cold and every hub
      * says there is nothing to seed from, and all of them leave join mode straight away.
      *
      * **An offer is adopted whether the store is cold or warm**, and adopting discards whatever
      * was there. A stale coil is the case the whole exchange is for — the empty store is its
      * degenerate form — and a hub only offers when the coil is too far behind to be walked
      * forward, so there is nothing left to preserve. See [[adopt]].
      */
    def marksAndWait(
        persistence: Persistence[IO]
    )(using config: OwnPeerPublic.Section): IO[(Join.Connected, JoinWait)] =
        for {
            markers <- Markers.derive(persistence, config.ownPeerId)
            startPoint <- adoptedStartPoint(persistence)
            connected = Join.Connected(
              block = markers.fastBlockMark,
              stack = markers.hardConfirmed
            )
            cold = markers.hardAckedStack.isEmpty && startPoint.isEmpty
        } yield (connected, if cold then JoinWait.Forever else JoinWait.Until(warmJoinWait))

    /** How long a coil liaison stays in join mode without an answer from its hub. */
    enum JoinWait:

        /** A cold coil: wait as long as it takes. Booting without an answer is the failure the
          * exchange exists to prevent — the coil would re-derive stack 0 from config, the head is
          * long past it, and nothing afterwards can reconcile the two.
          */
        case Forever

        /** A coil with history: proceed after this long. It has somewhere to walk forward from, so
          * a working node is not blocked on an unreachable hub.
          */
        case Until(after: FiniteDuration)

    /** How long a coil peer that already has history waits for its hub before booting anyway. Not
      * configurable: past it the node boots and catches up, so the only cost of the wait being
      * wrong is a slower start or a missed seeding that the next reconnect offers again.
      */
    val warmJoinWait: FiniteDuration = 30.seconds

    /** How often a cold coil says it is still waiting — the log line is what tells an operator the
      * hub is the problem.
      */
    val coldJoinReportEvery: FiniteDuration = 10.seconds
}

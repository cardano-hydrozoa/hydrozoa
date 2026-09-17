package hydrozoa.multisig.consensus

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.{AdoptedStartPoint, JournalKey, JournalValue, Persistence, StoreKey, WriteBatch}

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

    type Config = JoinOfferVerifier.Config & CardanoNetwork.Section

    /** Check an offer and, if it holds up, seed the store from it.
      *
      * **Nothing is trusted because it arrived.** The state is imported first so the ledger can
      * report what it actually reached, and those digests — never the ones travelling with the
      * bytes — are what [[JoinOfferVerifier]] checks against the head peers' signed certificate. A
      * refusal leaves the store untouched and fails the boot: a coil that cannot verify where it is
      * being put must not start there.
      *
      * ⚠️ The import lands before the store write, because the check needs the imported ledger's
      * digests. A crash between the two leaves a ledger holding state that no start point points
      * at; the next boot finds no start point, so it rejoins from scratch and re-imports.
      */
    def adopt(
        offer: Join.Offer,
        persistence: Persistence[IO],
        ledger: L2Ledger[IO]
    )(using config: Config): IO[Unit] =
        for {
            digests <- ledger.importState(offer.state).value.flatMap(IO.fromEither)
            verdict <- JoinOfferVerifier.verify(offer.settlement, offer.sec, digests)
            _ <- IO.fromEither(verdict)
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
                      ownHardAckStart = offer.ownHardAck
                    )
                  )
                  .put(StoreKey.Treasury)(offer.settlement.treasuryProduced)
                  .put(StoreKey.EvacuationMap(lastBlockNum))(map)
                  // The fast side resumes through `JointLedger.doneAt`, which reads all three of
                  // these at the anchor block. The coil pulls from `lastBlockNum + 1`, so this is
                  // the one block it will never be served.
                  .put(JournalKey.Block(lastBlockNum))(JournalValue(stamp, offer.block))
                  .put(StoreKey.DepositMap(lastBlockNum))(offer.deposits)
                  .put(StoreKey.L2CommandNumber(lastBlockNum))(offer.state.commandNumber)
            )
        } yield ()

    /** The start point this peer was seeded at, or `None` for a peer that produced its own history
      * — every head peer, and any coil peer that bootstrapped stack 0.
      */
    def adoptedStartPoint(persistence: Persistence[IO]): IO[Option[AdoptedStartPoint]] =
        persistence.get(StoreKey.StartPoint)
}

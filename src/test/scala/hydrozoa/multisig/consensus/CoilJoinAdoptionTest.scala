package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import cats.syntax.contravariant.*
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, Population}
import hydrozoa.multisig.consensus.liaison.BatchNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.{SettlementTx, TxSignature, genSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerCommand}
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{InMemoryBackendStore, JournalKey, JournalValue, Markers, Persistence, PersistenceEventFormat, StoreKey}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import test.MinorBlocks

/** Adopting a start point into a cold store — [[CoilJoin.adopt]] — and what the store reads as
  * afterwards.
  *
  * ⚠️ **The assertions that matter are the boot seams, not the writes.** A seeded coil has no own
  * hard-ack and no `BlockResult`, so both recovery paths would otherwise treat it as cold: the slow
  * side would re-bootstrap stack 0 and the fast side would rewind the ledger it just imported. The
  * store looking right is necessary; the two `recover` calls returning the adopted anchor is the
  * thing that makes it a joined peer rather than a corrupted one.
  */
class CoilJoinAdoptionTest extends AnyFunSuite {

    private val env: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private val nodeConfig: NodeConfig = env.nodeConfigs(HeadPeerNumber.zero)

    private given CoilJoin.Config = nodeConfig

    private val startStack = StackNumber(7)
    private val lastBlockNum = BlockNumber(12)
    private val offeredAck = HardAckNumber(5)

    private val donor: IO[EutxoL2Ledger] =
        InMemoryL2Store.create.flatMap(EutxoL2Ledger(nodeConfig, _))

    private def freshLedger: IO[EutxoL2Ledger] =
        InMemoryL2Store.create.flatMap(EutxoL2Ledger(nodeConfig, _))

    /** The settlement before any head peer signs it — what the verifier must refuse. */
    private val unsignedSettlement: SettlementTx =
        genSettlementTxSeqBuilder(env.headConfig)()
            .pureApply(Gen.Parameters.default, Seed(1L))
            .result match {
            case Left(e)  => throw RuntimeException(s"settlement build failed: $e")
            case Right(s) => s.settlementTx
        }

    private def withTreasury(s: SettlementTx, t: MultisigTreasuryUtxo): SettlementTx = s match {
        case x: SettlementTx.NoPayouts             => x.copy(treasuryProduced = t)
        case x: SettlementTx.WithOnlyDirectPayouts => x.copy(treasuryProduced = t)
        case x: SettlementTx.WithRollouts          => x.copy(treasuryProduced = t)
    }

    private val initTreasury: MultisigTreasuryUtxo = nodeConfig.initializationTx.treasuryProduced

    /** A genuine multisigned settlement carrying the head's **init** treasury.
      *
      * The treasury and the evacuation map are not independent: `StackComposer.State.recover`
      * checks the balance identity between them on the pair it boots from, and adoption inherits
      * that check. A generated treasury and a genesis map do not balance, and a coil is never
      * offered a pair that does not — the settlement that produced a treasury was built from the
      * very map beside it. The init pair is the one coherent pair a fixture can name.
      */
    private val settlement: SettlementTx =
        val signed =
            unsignedSettlement.txLens.replace(env.multisignTx(unsignedSettlement.tx))(
              unsignedSettlement
            )
        withTreasury(signed, initTreasury)

    /** An offer whose state is a real export from a genesis donor, so the digests line up with the
      * settlement the fixture head config produced.
      */
    private def offer(ownHardAck: HardAckNumber = offeredAck): IO[Join.Offer] =
        for {
            d <- donor
            exported <- d.exportStateAt(L2CommandNumber.zero).value
            block <- MinorBlocks.brief(env.headConfig, lastBlockNum: Int)
        } yield Join.Offer(
          startStack = startStack,
          cursors = Population.Get(
            batchNum = BatchNumber.zero,
            block = lastBlockNum.increment,
            blockCeiling = lastBlockNum.increment,
            stack = startStack.increment,
            stackCeiling = startStack.increment,
            requests = Map.empty,
            requestCeilings = Map.empty,
            softAcks = Map.empty,
            headHardAcks = Map.empty,
            coilHardAcks = Map.empty,
            coilHardAckCeiling = startStack.increment
          ),
          ownHardAck = ownHardAck,
          settlement = settlement,
          sec = None,
          state = exported.toOption.get,
          block = block,
          deposits = DepositsMap.empty
        )

    private def withStore[A](f: Persistence[IO] => IO[A]): A =
        val tracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(tracer)
            .use(backend => Persistence.fromBackend(backend, tracer).flatMap(f))
            .unsafeRunSync()

    private def adopted[A](f: (Persistence[IO], EutxoL2Ledger) => IO[A]): A =
        withStore(p =>
            for {
                ledger <- freshLedger
                o <- offer()
                _ <- CoilJoin.adopt(o, p, ledger)
                a <- f(p, ledger)
            } yield a
        )

    test("adoption records the start point it was seeded at") {
        val mark = adopted((p, _) => p.get(StoreKey.StartPoint))
        val _ = assert(mark.map(_.startStack).contains(startStack))
        val _ = assert(mark.map(_.lastBlockNum).contains(lastBlockNum))
        assert(mark.map(_.ownHardAckStart).contains(offeredAck))
    }

    test("a seeded store fabricates no own hard-ack and no block result") {
        // Nothing is written to the two journals that would be read as this peer's own
        // production: the `HardAck` journal its hub pulls from, and the `BlockResult` spine. A row
        // in either would exist only to be misread.
        val outcome = adopted((p, _) =>
            (
              Markers.derive(p, nodeConfig.ownPeerId),
              Markers.recoverFastBlockMark(p.backend)
            ).tupled
        )
        val (markers, blockResults) = outcome
        val _ = assert(markers.hardAcked.isEmpty, "adoption wrote an ack this peer never made")
        val _ = assert(
          markers.hardAckedStack.isEmpty,
          "the slow anchor must come from the start point, not from a fabricated ack"
        )
        val _ = assert(blockResults.isEmpty, "adoption wrote a block result this peer never made")
        // ...but the FAST anchor is reported, because the peer really does hold that block. It is
        // the replay floor: leave it empty and replay re-feeds the adopted anchor as new input.
        assert(
          markers.fastBlockMark.contains(lastBlockNum),
          s"fast anchor should be the adopted block, got ${markers.fastBlockMark}"
        )
    }

    test("the slow side opens at the adopted stack instead of re-bootstrapping stack 0") {
        val recovered = adopted((p, _) =>
            Markers
                .derive(p, nodeConfig.ownPeerId)
                .flatMap(m =>
                    StackComposer.State.recover(p, m.hardAcked, m.hardConfirmed, m.hardAckedStack)(
                      using nodeConfig
                    )
                )
        )
        val _ =
            assert(recovered.isDefined, "a seeded store must not read as cold — that re-bootstraps")
        val _ = assert(recovered.map(_.lastClosedStackNum).contains(startStack))
        val _ = assert(recovered.map(_.lastClosedBlockNum).contains(lastBlockNum))
        assert(
          recovered.map(_.nextOwnHardAckNum).contains(offeredAck),
          "the coil must number its first ack where its hub is already pulling from"
        )
    }

    test("the fast side resumes at the adopted block instead of rewinding the imported ledger") {
        val done = adopted((p, ledger) =>
            Markers
                .derive(p, nodeConfig.ownPeerId)
                .flatMap(m =>
                    hydrozoa.multisig.ledger.joint.JointLedger.State.recover(
                      p,
                      ledger,
                      m.fastBlockMark,
                      nodeConfig.initialEvacuationMap,
                      nodeConfig.initialL2StateHash,
                      m.evacuationMapMark,
                      nodeConfig.l2ParamsHash
                    )(using nodeConfig)
                )
        )
        val _ = assert(done.isDefined, "no anchor means restoreTo(zero), which discards the import")
        assert(done.map(_.previousBlockHeader.blockNum).contains(lastBlockNum))
    }

    test("the offer's cursors are persisted, not recomputed at home") {
        // The gap this test closes: a lane cursor is normally `max(journal) + 1`, and a seeded
        // coil's journals are empty — so without carrying these the coil pulls from the beginning
        // of a history its hub has very likely pruned. The hard-ack indices in particular cannot
        // be derived from the start point at all.
        val mark = adopted((p, _) => p.get(StoreKey.StartPoint))
        val cursors = mark.map(_.cursors)
        val _ = assert(cursors.map(_.block).contains(lastBlockNum.increment))
        assert(cursors.map(_.stack).contains(startStack.increment))
    }

    test("adopting into a warm store discards what was there") {
        // The case the exchange exists for: a stale coil, too far behind for its hub to walk it
        // forward. Its ledger cannot take an import while it holds anything, and its old journals
        // would anchor recovery below the start point — on history the hub no longer has. So the
        // old data goes, and the assertion is that it is actually gone rather than merged with.
        val outcome = withStore(p =>
            for {
                ledger <- freshLedger
                // A previous life: an own hard-ack and a block spine far below the start point.
                stamp <- p.arrivalStamp
                _ <- p.put(JournalKey.HardAck(nodeConfig.ownPeerId, HardAckNumber(0)))(
                  JournalValue(
                    stamp,
                    HardAck(
                      ackId = HardAckId(nodeConfig.ownPeerId, HardAckNumber(0)),
                      stackNum = StackNumber(1),
                      payload = HardAck.Round2Payload
                          .Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
                    )
                  )
                )
                before <- Markers.derive(p, nodeConfig.ownPeerId)
                o <- offer()
                _ <- CoilJoin.adopt(o, p, ledger)
                after <- Markers.derive(p, nodeConfig.ownPeerId)
                mark <- p.get(StoreKey.StartPoint)
            } yield (before, after, mark)
        )
        val (before, after, mark) = outcome
        val _ = assert(before.hardAckedStack.contains(StackNumber(1)), "fixture was not warm")
        val _ = assert(
          after.hardAckedStack.isEmpty,
          "the old own-ack survived the wipe and would anchor recovery below the start point"
        )
        assert(mark.map(_.startStack).contains(startStack))
    }

    test("a ledger that has already applied commands can still adopt") {
        // `importState` adopts only into a ledger that has applied nothing, so without the wipe a
        // stale coil — the case the exchange exists for — could never be seeded at all.
        val outcome = withStore(p =>
            for {
                ledger <- freshLedger
                _ <- ledger.applyDepositDecisions(
                  L2CommandNumber(1L),
                  L2LedgerCommand.ApplyDepositDecisions(
                    blockNumber = BlockNumber(1),
                    blockCreationEndTime = BigInt(1),
                    absorbedDeposits = Nil,
                    rejectedDeposits = Nil
                  )
                )
                o <- offer()
                result <- CoilJoin.adopt(o, p, ledger).attempt
                mark <- p.get(StoreKey.StartPoint)
            } yield (result, mark)
        )
        val (result, mark) = outcome
        val _ = assert(result.isRight, s"a ledger with history could not be seeded: $result")
        assert(mark.map(_.startStack).contains(startStack))
    }

    test("an offer that does not verify is refused before anything is destroyed") {
        // The property the adoption order exists for. Adopting wipes, so an offer anyone could
        // forge must be refused while the coil still has everything — otherwise a bad actor that
        // cannot seed a coil can still empty it.
        val outcome = withStore(p =>
            for {
                ledger <- freshLedger
                stamp <- p.arrivalStamp
                _ <- p.put(JournalKey.HardAck(nodeConfig.ownPeerId, HardAckNumber(0)))(
                  JournalValue(
                    stamp,
                    HardAck(
                      ackId = HardAckId(nodeConfig.ownPeerId, HardAckNumber(0)),
                      stackNum = StackNumber(1),
                      payload = HardAck.Round2Payload
                          .Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
                    )
                  )
                )
                o <- offer()
                // No head-peer witness at all, so the settlement does not satisfy the head's
                // native script and `checkSettlementValid` refuses it.
                result <- CoilJoin
                    .adopt(o.copy(settlement = unsignedSettlement), p, ledger)
                    .attempt
                mark <- p.get(StoreKey.StartPoint)
                after <- Markers.derive(p, nodeConfig.ownPeerId)
            } yield (result, mark, after)
        )
        val (result, mark, after) = outcome
        val _ = assert(result.isLeft, "a settlement that does not verify must fail the boot")
        val _ = assert(mark.isEmpty, "a refused offer must leave no start point behind")
        assert(
          after.hardAckedStack.contains(StackNumber(1)),
          "a refused offer wiped the store it was refused by"
        )
    }

    test("an offer refused on its state, not its signatures, destroys nothing either") {
        // The half the signatures do not cover. `verifyCertificate` passes here — the settlement
        // is the genuine multisigned one — and the offer still fails, on the only thing that
        // binds the certificate to the bytes beside it. Before GUM-354 that comparison ran after
        // the wipe, so reaching it cost the coil its store.
        val certifiesAnotherState = withTreasury(
          settlement,
          initTreasury.copy(
            datum = initTreasury.datum
                .copy(l2StateHash = ByteString.fromArray(Array.fill[Byte](32)(0x99.toByte)))
          )
        )
        val outcome = withStore(p =>
            for {
                ledger <- freshLedger
                stamp <- p.arrivalStamp
                _ <- p.put(JournalKey.HardAck(nodeConfig.ownPeerId, HardAckNumber(0)))(
                  JournalValue(
                    stamp,
                    HardAck(
                      ackId = HardAckId(nodeConfig.ownPeerId, HardAckNumber(0)),
                      stackNum = StackNumber(1),
                      payload = HardAck.Round2Payload
                          .Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
                    )
                  )
                )
                o <- offer()
                result <- CoilJoin
                    .adopt(o.copy(settlement = certifiesAnotherState), p, ledger)
                    .attempt
                mark <- p.get(StoreKey.StartPoint)
                after <- Markers.derive(p, nodeConfig.ownPeerId)
            } yield (result, mark, after)
        )
        val (result, mark, after) = outcome
        val _ =
            assert(
              result.isLeft,
              s"a state the certificate does not commit to was adopted: $result"
            )
        val _ = assert(mark.isEmpty, "a refused offer must leave no start point behind")
        assert(
          after.hardAckedStack.contains(StackNumber(1)),
          "a refused offer wiped the store it was refused by"
        )
    }
}

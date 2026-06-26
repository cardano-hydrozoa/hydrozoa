package hydrozoa.timingviz

import cats.effect.{IO, Ref}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationStartTime
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent as MRMEvent
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** Bridge from Hydrozoa's typed [[MRMEvent]] tracer surface to the timing visualizer's [[Command]]
  * alphabet.
  *
  * Two flavors:
  *
  *   - [[stateless]] — pure mapping; one MRM event → one optional `Command`. Good enough for events
  *     that carry both endpoints of their timing in one shot.
  *   - [[make]] — stateful; correlates events that emit start and end separately
  *     (`BlockStarted`/`BlockCompleting`) so a block becomes a proper-duration `ObserveBlock`.
  *     Wraps a Ref internally.
  *
  * Wiring example (Main.scala):
  *
  * {{{
  *   import cats.kernel.Monoid
  *   import hydrozoa.timingviz.{VizClient, VizTracer}
  *
  *   VizClient.open(uri"ws://localhost:8765/ws").use { send =>
  *     for
  *       vizTr <- VizTracer.make(send)
  *       combined = Monoid[ContraTracer[IO, MRMEvent]].combine(slf4jTr, vizTr)
  *       _ <- runRegimeManagerWith(combined)
  *     yield ()
  *   }
  * }}}
  */
object VizTracer:

    /** Per-tracer in-memory state for cross-event correlation. */
    private final case class Buffer(
        pendingBlocks: Map[BlockNumber, BlockCreationStartTime]
    )
    private object Buffer:
        val empty: Buffer = Buffer(Map.empty)

    /** Stateful tracer: correlates start/end events for proper intervals. */
    def make(send: Command => IO[Unit]): IO[ContraTracer[IO, MRMEvent]] =
        Ref.of[IO, Buffer](Buffer.empty).map { ref =>
            ContraTracer { (event: MRMEvent) => handle(event, ref, send) }
        }

    /** Stateless tracer: only emits a `Command` when one MRM event carries enough information.
      * Loses block-creation-start when the block is observed via `BlockCompleting` alone — use
      * [[make]] if you want proper-duration block bars.
      */
    def stateless(send: Command => IO[Unit]): ContraTracer[IO, MRMEvent] =
        val cmdSink: ContraTracer[IO, Command] = ContraTracer(send)
        cmdSink.traceMaybe(commandFor)

    /** Pure mapping for the stateless flavor. Exposed so tests can drive it directly. */
    def commandFor(event: MRMEvent): Option[Command] = event match
        case MRMEvent.JointLedger(JointLedgerEvent.BlockCompleting(num, end, _, _)) =>
            // Zero-duration bar; the stateless mapping has no start time to pair with.
            val endQ = end.convert
            Some(
              Command.ObserveBlock(
                id = Ids.BlockId(s"blk-${num.toString}"),
                creation = Interval(endQ, endQ),
                kind = BlockKind.Major
              )
            )
        case _ => None

    // --- stateful handler -------------------------------------------------------------------

    private def handle(
        event: MRMEvent,
        ref: Ref[IO, Buffer],
        send: Command => IO[Unit]
    ): IO[Unit] = event match
        case MRMEvent.JointLedger(JointLedgerEvent.BlockStarted(num, start)) =>
            ref.update(b => b.copy(pendingBlocks = b.pendingBlocks + (num -> start)))

        case MRMEvent.JointLedger(JointLedgerEvent.BlockCompleting(num, end, _, _)) =>
            ref
                .modify(b =>
                    (
                      b.copy(pendingBlocks = b.pendingBlocks - num),
                      b.pendingBlocks.get(num)
                    )
                )
                .flatMap {
                    case Some(start) =>
                        send(
                          Command.ObserveBlock(
                            id = Ids.BlockId(s"blk-${num.toString}"),
                            creation = Interval(start.convert, end.convert),
                            kind = BlockKind.Major
                          )
                        )
                    case None =>
                        // No matching BlockStarted — fall back to the stateless approximation.
                        send(
                          Command.ObserveBlock(
                            id = Ids.BlockId(s"blk-${num.toString}"),
                            creation = Interval(end.convert, end.convert),
                            kind = BlockKind.Major
                          )
                        )
                }

        case _ => IO.unit

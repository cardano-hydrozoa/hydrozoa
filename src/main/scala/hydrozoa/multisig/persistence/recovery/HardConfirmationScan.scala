package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.stack.{StackEffects, StackNumber}
import hydrozoa.multisig.persistence.{BackendStore, Cf, StoreKey}
import java.nio.ByteBuffer

/** Range-scan the [[Cf.HardConfirmation]] CF — `SlowConsensusActor`'s per-stack multisigned effects
  * — from a stack-number floor (inclusive) to the end, decoding each entry to its typed
  * [[StackEffects.HardConfirmed]].
  *
  * `CardanoLiaison.State.recover` folds the full sequence (from `StackNumber.zero`) through the
  * same kernels the live `Stack.HardConfirmed` path uses, rebuilding its submission index.
  * `HardConfirmation` is a non-lane CF — entries carry no arrival-stamp prefix, so the raw value
  * bytes decode directly. See `design/recovery-implementation-plan.md` R2-bnd.
  */
object HardConfirmationScan:

    /** The persisted `StackEffects.HardConfirmed`s with `stackNum >= fromInclusive`, in ascending
      * stack order.
      */
    def scanFrom(
        backend: BackendStore[IO],
        fromInclusive: StackNumber
    )(using CardanoNetwork.Section): IO[List[StackEffects.HardConfirmed]] =
        val seek = StoreKey.HardConfirmation(fromInclusive).encode
        backend.cursor(Cf.HardConfirmation, seek).use { cursor =>
            def loop(acc: List[StackEffects.HardConfirmed]): IO[List[StackEffects.HardConfirmed]] =
                cursor.next.flatMap {
                    case None => IO.pure(acc.reverse)
                    case Some((keyBytes, valueBytes)) =>
                        val key =
                            StoreKey.HardConfirmation(StackNumber(ByteBuffer.wrap(keyBytes).getInt))
                        loop(key.decodeValue(valueBytes) :: acc)
                }
            loop(Nil)
        }

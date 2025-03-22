package hydrozoa.l2.ledger

import hydrozoa.l2.ledger.event.AnyEventL2

/** @tparam TUtxosSetOpaque
  *   An opaque type for representing utxo set. Used to move a state from "block building" ledger to
  *   the main ledger when applying L2 block effects.
  * @tparam TUtxosSetNonOpaque
  *   A non-opaque type that can be scrutinized. Used also in TEventL2, see 'submit'.
  * @tparam TEventHash
  *   A type for hashes of L2 events 'TEventL2'.
  * @tparam G
  *   The public type for genesis events.
  * @tparam T
  *   The public type for transactions.
  * @tparam W
  *   The public type for withdrawals.
  * @tparam TEventL2
  *   Type for L2 events.
  */
trait L2Ledger[
    TUtxosSetOpaque,
    TUtxosSetNonOpaque,
    +TEventHash,
    G,
    T,
    W,
    TEventL2 <: AnyEventL2[
      TEventHash,
      G,
      T,
      W,
      TUtxosSetNonOpaque
    ]
]:

    /** @return
      *   Returns an opaque copy of active utxos.
      */
    def getUtxosActive: TUtxosSetOpaque

    /** @return
      *   Whether a ledger's utxos set is empty.
      */
    def isEmpty: Boolean

    /** Replaces the active set of utxos with a provided set.
      * @param activeState
      *   A new set of utxos.
      */
    def replaceUtxosActive(activeState: TUtxosSetOpaque): Unit

    /** Removes all active utxos from the ledger.
      * @return
      *   A copy ofactive utxos as a non-opaque type.
      */
    def flush: TUtxosSetNonOpaque

    /** Tries to submit an event. returning its
      * @param event
      * @tparam SomeEvent
      * @return
      *   An event hash and an error in case of failure, event hash and (dependently on the type of
      *   event) diff of generated/withdrawn utxos.
      */
    def submit[SomeEvent <: TEventL2](
        event: SomeEvent
    ): Either[(TEventHash, String), (TEventHash, event.UtxosDiff)]

/** @tparam E
  *   Type for events that a verifier can verify.
  */
trait Verifier[-E]:
    def isValid(event: E): Boolean

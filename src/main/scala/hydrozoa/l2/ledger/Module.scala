package hydrozoa.l2.ledger

import hydrozoa.l2.ledger.simple.UtxosSet
import hydrozoa.{OutputL2, TxId, UtxoIdL2}

sealed trait LedgerPurpose

sealed trait HydrozoaHeadLedger extends LedgerPurpose
sealed trait BlockProducerLedger extends LedgerPurpose
sealed trait MBTSuiteLedger extends LedgerPurpose

trait L2LedgerModule[InstancePurpose <: LedgerPurpose, LedgerUtxoSetOpaque]:

    type LedgerEvent
    type LedgerEventHash

    type SubmissionError

    /** @return
      *   Checks whether a ledger's utxos set is empty.
      */
    def isEmpty: Boolean

    /** @return
      *   Returns an opaque copy of active utxos.
      */
    def getUtxosActive: LedgerUtxoSetOpaque

    /** Replaces the active set of utxos with a provided set.
      *
      * @param activeState
      *   A new set of utxos.
      */
    def replaceUtxosActive(activeState: LedgerUtxoSetOpaque): Unit

    // Returns utxo state. Used only in L2 state endpoint for now.
    def getState: UtxosSet

    def getOutput(utxoId: UtxoIdL2): OutputL2

    /** Removes all active utxos from the ledger.
      *
      * @return
      *   Clears up utxo set and returns a copy of active utxos as a non-opaque type.
      */
    def flush(): UtxosSet

    /** Tries to submit an event returning the event's hash and an error in case of failure or
      * event's hash TODO: (and diff of generated/withdrawn utxos) - use wrapper for this.
      *
      * @param event
      * @tparam SomeEvent
      * @return
      */
    def submit(
        event: LedgerEvent
    ): Either[(TxId, SubmissionError), (TxId, (UtxosSet, UtxosSet))]

    /** Makes a copy of the current ledger for block production purposes.
      *
      * @param ev
      *   evidence that we are cloning Hydrozoa ledger, not its clone
      * @return
      *   cloned ledger
      */
    def cloneForBlockProducer()(using
        InstancePurpose =:= HydrozoaHeadLedger
    ): L2LedgerModule[BlockProducerLedger, LedgerUtxoSetOpaque]

    def toLedgerEvent(event: L2Transaction | L2Withdrawal): LedgerEvent

    def addGenesisUtxos(utxos: UtxosSet): Unit

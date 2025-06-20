package hydrozoa.l2.ledger

import hydrozoa.{OutputL2, TxId, UtxoIdL2, UtxoSetL2}

sealed trait LedgerPurpose

sealed trait HydrozoaHeadLedger extends LedgerPurpose
sealed trait BlockProducerLedger extends LedgerPurpose
sealed trait MBTSuiteLedger extends LedgerPurpose

trait L2LedgerModule[InstancePurpose <: LedgerPurpose, LedgerUtxoSetOpaque]:

    /** Type for things, that can be submitted to the ledger.
      */
    type LedgerTransaction

    /** Type for error responses
      */
    type SubmissionError

    /** @return
      *   Checks whether a ledger's utxos set is empty.
      */
    def isEmpty: Boolean

    /** @return
      *   Returns an opaque copy of active utxos.
      */
    def getUtxosActive: LedgerUtxoSetOpaque
    
    def getUtxosActiveCommitment: IArray[Byte] 
    
    // Returns utxo state. Used only in L2 state endpoint for now.
    def getState: UtxoSetL2

    /** Replaces the active set of utxos with a provided set.
      *
      * @param activeState
      *   A new set of utxos.
      */
    def replaceUtxosActive(activeState: LedgerUtxoSetOpaque): Unit

    /** Allows adding arbitrary genesis UTxOs. 
     * @param utxos
     */
    def addGenesisUtxos(utxos: UtxoSetL2): Unit
    
    def getOutput(utxoId: UtxoIdL2): OutputL2

    /** Removes all active utxos from the ledger.
      *
      * @return
      *   Clears up utxo set and returns a copy of active utxos as a non-opaque type.
      */
    def flushAndGetState: UtxoSetL2

    /** Tries to submit an event returning the event's hash and an error in case of failure or
      * event's hash TODO: (and diff of generated/withdrawn utxos) - use wrapper for this.
      *
      * @param event
      * @tparam SomeEvent
      * @return
      */
    def submit(
        event: LedgerTransaction
    ): Either[(TxId, SubmissionError), (TxId, (UtxoSetL2, UtxoSetL2))]

    /**
     * Transforms Hydrozoa's tx/withdrawal into ledger format.
     * @param tx
     * @return
     */
    def toLedgerTransaction(tx: L2Transaction | L2Withdrawal): LedgerTransaction

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


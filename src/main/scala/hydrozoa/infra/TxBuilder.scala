package hydrozoa.infra

import hydrozoa.infra.transitionary.emptyTxBody
import scalus.cardano.ledger.*

import scala.util.control.TailCalls.{TailRec, done}

trait TxBuilder {
    // The Tx Builder "recipe" type contains all the information needed
    // to kick off the building pipeline
    type Recipe

    // Queries are thing that can touch the network, disk, etc. In Haskell, they would be in IO.
    // They can fail. We want to isolate thing like "network failures" or "disk" failures here.
    type QueryResult
    type QueryError
    def query(recipe: Recipe): Either[QueryError, QueryResult]

    // Calculations are pure functions that can fail, such as parsing. We differentiate them from
    // queries because (for us) queries are not pure. The failures we want to isolate here are things
    // like "the query returned a UTxO at the right address, but we expected an inline datum and got a datum hash",
    // or "the redeemer provided won't satisfy on-chain logic".
    type CalculationResult
    type CalculationError
    def calculate(queryResult: QueryResult): Either[CalculationError, CalculationResult]

    // The "tx initializer" is a non-recursive entry-point for constructing an initial Tx based on the calculation
    // result and an "entrypoint" transaction . We're considering implementing it as a first step for both efficiency
    // purposes and to make it easier to test. This is (essentially) just an endomorphism over `Transaction`; the errors
    // here won't be due to recursion.
    type InitializationErrorType
    // In most cases, the initializer result will just be a Transaction.
    // However, we make it generic in order to write info out of the build process
    // for diagnostics or logging
    type InitializerResult
    def txInitializer(
        calculationResult: CalculationResult,
        entryTx: Transaction
    ): Either[InitializationErrorType, InitializerResult]

    // N.B.: Should include dummy signature for tx sizing purposes
    // This is heavily inspired from sc-tools:
    // https://github.com/j-mueller/sc-tools/blob/fedd88a0fd8a053d1bd37faa6babc452260e88f8/src/base/lib/Convex/BuildTx.hs#L230-L252
    // The reason we need a fixed point are two-fold:
    //   - General balancing
    //   - Certain optimizations used in plutus smart contract design require including data into redeemers or datums
    //     that cannot be known until the full transaction is built. For example, a common optimization is to
    //     pass the index of a given utxo in the (lexicographically sorted) input list in a redeemer to a script
    //     so that the script does not need to parse values to "search" for a specific token.
    //     Instead, the known index is passed in the redeemer, and the script only needs to verify that the pointed-to
    //     utxo contains the correct token.
    //
    // The downside is that mis-constructing this function can lead to infinite loops.
    type FinalizerError
    // In most cases, the finalizer result will just be Transaction. However, we make it generic
    // in order to write info out of the build process for diagnostics or
    // logging
    type FinalizerResult

    def txFinalizer(
        cr: CalculationResult,
        initRes: InitializerResult
    ): TailRec[Either[FinalizerError, FinalizerResult]]

    // Have a smart constructor to add dummy signatures from specified pubkeys so that we can re-use
    // transaction sizing machinery.
    // QUESTION: Should this be a value of the class? Or should it be passed as an argument to runTxBuilder?
    val entryTx: Transaction = Transaction(
      body = KeepRaw(emptyTxBody),
      witnessSet = TransactionWitnessSet.empty,
      isValid = false,
      auxiliaryData = None
    ) // emptyTxBody

    // This is the main (concrete) method of this class. It runs queries,
    // performs the requisite calculations, and builds the Tx. The dataflow is
    // essentially the following (with `Either`'s threaded throughout):
    /*

       Recipe
         |
         |
         v
    ============
    |   Query  |
    ============
         |
         | QueryResult
         |
         v
    ==============
    | calculate  |
    ==============
         |                     entryTx : Transaction
         | CalcResult           |
         |                      v
         |------------> |===============|
         |              |  initializer  |
         |              |===============|
         |                 |
         |                 |  InitializerResult
         |                 v
         |             |=================|
         |------------>|   finalizer     |<-----|
                       |=================|      |
                                |               | (recursive case)
                                |               |
                  FinalizerRes  |               |
                                |               |
                                |---------------|
                                |
                                | (non recursive case)
                                v
                 finalRes : FinalizerRes

     */
    def runTxBuilder(recipe: Recipe): Either[
      QueryError | CalculationError | InitializationErrorType | FinalizerError,
      FinalizerResult
    ] = {
        for
            qr <- this.query(recipe)
            cr <- this.calculate(qr)
            initRes <- this.txInitializer(cr, this.entryTx)
            finalRes <- this.txFinalizer(cr = cr, initRes = initRes).result
        yield finalRes
    }

    // Helper functions
    // A function usable in the finalizer fixed-point that simply returns the initial transaction
    def idFinalizer(irToFr: InitializerResult => FinalizerResult)(
        cr: CalculationResult,
        initRes: InitializerResult
    ): TailRec[Either[FinalizerError, FinalizerResult]] = done(Right(irToFr(initRes)))
}

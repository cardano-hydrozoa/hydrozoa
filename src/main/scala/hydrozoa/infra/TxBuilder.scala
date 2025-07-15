package hydrozoa.infra

import hydrozoa.infra.transitionary.emptyTxBody
import scalus.cardano.ledger.{KeepRaw, Transaction, TransactionBody, TransactionWitnessSet}

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
    def calculate(queryResult: QueryResult): Either[CalculationError, CalculationResult]
    type CalculationResult
    type CalculationError

    // The "tx initializer" is a non-recursive entry-point for constructing an initial Tx based on the calculation
    // result and an "entrypoint" transaction . We're considering implementing it as a first step for both efficiency
    // purposes and to make it easier to test. This is (essentially) just an endomorphism over `Transaction`; the errors
    // here won't be due to recursion.
    type InitializationErrorType
    def txInitializer(
        calculationResult: CalculationResult,
        initializer: Transaction
    ): Either[InitializationErrorType, Transaction]

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
    type FixedPointError
    def txFixedPoint(calculationResult: CalculationResult): (
        resultTxBody: Either[FixedPointError, Transaction],
        initialTxBody: Transaction
    ) => Either[FixedPointError, Transaction]

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
         |                      |
         |                      | Transaction
         |                      v
         |             |=================|
         |------------>|   fixedPoint    |<-----|
                       |=================|      |
                                |               | (recursive case)
                  Transaction   |               |
                                |---------------|
                                |
                                | (non recursive case)
                                v
                              finalTx : Transaction

     */
    def runTxBuilder(recipe: Recipe): Either[
      QueryError | CalculationError | InitializationErrorType | FixedPointError,
      Transaction
    ] = {
        val qr: QueryResult = this.query(recipe) match {
            case Left(err) => return Left(err)
            case Right(r)  => r
        }

        val cr: CalculationResult = this.calculate(qr) match {
            case Left(err) => return Left(err)
            case Right(r)  => r
        }

        val initTxBody: Transaction = this.txInitializer(cr, this.entryTx) match {
            case Left(err) => return Left(err)
            case Right(r)  => r
        }

        // We create a fixed point based on the result of the calculations
        val finalTx = {
            val fp = this.txFixedPoint(cr)
            lazy val result: Either[FixedPointError, Transaction] = {
                fp(result, initTxBody)
            }
            result
        }
        finalTx
    }

}

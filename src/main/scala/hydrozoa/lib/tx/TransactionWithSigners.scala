package hydrozoa.lib.tx

import scalus.cardano.ledger.AddrKeyHash
//
//import hydrozoa.lib.tx.StakeCredential
//import hydrozoa.{emptyTransaction, txBodyL}
//import hydrozoa.lib.tx.TransactionWithSigners.{ValidSignersPartition, mkUnsafe}
//import monocle.Focus.{focus, refocus}
//import monocle.Lens
//import scalus.cardano.ledger.{AddrKeyHash, Certificate, PolicyId, ProposalProcedure, TaggedOrderedSet, Transaction, TransactionInput, Voter, Withdrawals}
//import scalus.|>
//
///** A transaction paired with the expected (and required) signers. This is necessary for accurate
//  * fee calculation.
//  *
//  * If the number of expected signers is too small compared to the number of actual signers, the
//  * transaction should fail with an insufficient fee error.
//  *
//  * If the number of expected signers is too large compared to the number of actual signers, the
//  * transaction may pass, but the fee will be over-paid.
//  *
//  * Note: this class is opaque. It is not (or should not be) possible to have [[ExpectedSigner]]s or
//  * [[PlutusSigner]]s that point to invalid transaction components.
//  */
//case class TransactionWithSigners private (tx: Transaction):
//
//
//object TransactionWithSigners:
//    val empty: TransactionWithSigners = TransactionWithSigners.mkUnsafe(emptyTransaction, Set.empty)
//
//    case class ValidSignersPartition(
//        /** Signers that have a signingPurpose that point to a valid transaction component */
//        resolvableExpectedSigners: Set[ExpectedSigner],
//        /** Signers that have a signingPurpose that does not point to a valid transaction component
//          */
//        danglingExpectedSigners: Set[ExpectedSigner],
//        /** key hashes in the requiredSigners field of the transaction body that can be resolved to
//          * one or more resolvable expected signers
//          */
//        resolvableRequiredSigners: Set[AddrKeyHash],
//        /** key hashes in the requiredSigners field of the transaction body that cannot be resolved
//          * to any resolvable expected signer.
//          */
//        danglingRequiredSigners: Set[AddrKeyHash]
//    )
//
//    private def partitionValidSignatures(
//        tx: Transaction,
//        expectedSigners: Set[ExpectedSigner]
//    ): ValidSignersPartition = {
//        // Invariant 1: Ensure that all expected signers resolve to real transaction components.
//        val (resolvableExpectedSigners, danglingExpectedSigners) = expectedSigners.foldLeft(
//          (Set.empty[ExpectedSigner], Set.empty[ExpectedSigner])
//        )((acc, signer) =>
//            if signer.signerPurpose.isIn(tx) then acc.focus(_._1).modify(_ + signer)
//            else acc.focus(_._2).modify(_ + signer)
//        )
//
//        // Invariant 2: ensure that all requiredSigners in the tx body refer to signatures in the valid expected signers
//        // set
//        val actualRequiredSigners: Set[AddrKeyHash] =
//            (tx |> txBodyL.refocus(_.requiredSigners).get).toSortedSet
//        val validRequiredHashes: Set[AddrKeyHash] =
//            resolvableExpectedSigners
//                .filter(_.isInstanceOf[PlutusSigner])
//                .map(_.signer)
//        val (resolvableRequiredSigners, danglingRequiredSigners) =
//            actualRequiredSigners.partition(validRequiredHashes.contains)
//
//        ValidSignersPartition(
//          resolvableExpectedSigners = resolvableExpectedSigners,
//          danglingExpectedSigners = danglingExpectedSigners,
//          resolvableRequiredSigners = resolvableRequiredSigners,
//          danglingRequiredSigners = danglingRequiredSigners
//        )
//    }
//
//    /** Create a transaction with expected signers, returning a Left if any dangling expected or
//      * required signers are present.
//      * @param tx
//      *   a transaction
//      * @param expectedSigners
//      *   a set of expected signers for this transaction
//      * @return
//      *   Left(invalidSigners) or Right(transactionWithExpectedAndRequiredSigners)
//      */
//    def mk(
//        tx: Transaction,
//        expectedSigners: Set[ExpectedSigner]
//    ): Either[ValidSignersPartition, TransactionWithSigners] = {
//        val signersPartition = partitionValidSignatures(tx, expectedSigners)
//        if signersPartition.danglingRequiredSigners.isEmpty && signersPartition.danglingExpectedSigners.isEmpty
//        then {
//            val txWithRS = tx |> txBodyL
//                .refocus(_.requiredSigners)
//                .replace(
//                  TaggedOrderedSet.from(signersPartition.resolvableRequiredSigners)
//                )
//
//            Right(TransactionWithSigners(txWithRS, signersPartition.resolvableExpectedSigners))
//        } else Left(signersPartition)
//    }
////
//    /** Silently drops any dangling expected or required signers
//      * @param tx
//      * @param expectedSigners
//      * @return
//      */
//    def mkUnsafe(tx: Transaction, expectedSigners: Set[ExpectedSigner]): TransactionWithSigners = {
//        val signersPartition = partitionValidSignatures(tx, expectedSigners)
//
//        val txWithRS = tx |> txBodyL
//            .refocus(_.requiredSigners)
//            .replace(TaggedOrderedSet.from(signersPartition.resolvableRequiredSigners))
//        TransactionWithSigners(txWithRS, signersPartition.resolvableExpectedSigners)
//    }
//
//    /** Lens using [[mkUnsafe]].
//      *
//      * This is safe to use only if you're modifying parts of the transaction that do not correspond
//      * to signatures. It will silently drop signatures that have been made invalid as a result of
//      * modifying the transaction.
//      */
//    val txwsTxUnsafeL: Lens[TransactionWithSigners, Transaction] = {
//        val getter: TransactionWithSigners => Transaction = (txws => txws.tx)
//        val setter: Transaction => TransactionWithSigners => TransactionWithSigners = {
//            (tx => txws => mkUnsafe(tx = tx, expectedSigners = txws.expectedSigners))
//        }
//        Lens(getter)(setter)
//    }
//
//    /** Lens using [[mkUnsafe]].
//      *
//      * This will silently drop signatures that are passed to the setter, but that do not correspond
//      * to valid signature components
//      */
//    val txwsSignersUnsafeL: Lens[TransactionWithSigners, Set[ExpectedSigner]] = {
//        val getter: TransactionWithSigners => Set[ExpectedSigner] = _.expectedSigners
//        val setter: Set[ExpectedSigner] => TransactionWithSigners => TransactionWithSigners =
//            (es => txws => mkUnsafe(tx = txws.tx, expectedSigners = es))
//        Lens(getter)(setter)
//    }

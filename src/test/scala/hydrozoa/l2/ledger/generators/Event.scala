package hydrozoa.l2.ledger.generators

import hydrozoa.*
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l2.ledger.{L2EventGenesis, L2EventTransaction}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.address
import monocle.syntax.all.*
import org.scalacheck.Gen
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State}
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3
import scalus.prelude.Option as SOption

/** Build dummy deposit datum from a pubkey, setting the L2 and refund addresses to the pkh address
 */
def depositDatumFromPeer(peer: TestPeer): Option[DatumOption] = {
    val v3Addr: v3.Address = v3
        .Address(
            credential = v3.Credential
                .PubKeyCredential(PubKeyHash(address(peer).payment.asHash)),
            SOption.None
        )
    Some(
        Inline(
            toData(
                DepositDatum(
                    address = v3Addr,
                    datum = SOption.None,
                    deadline = 100,
                    refundAddress = v3Addr,
                    refundDatum = SOption.None
                )
            )
        )
    )
}

/** Generate a single, semantically valid but fully synthetic deposit for inclusion into a genesis
 * event
 */
def genDepositFromPeer(peer: TestPeer): Gen[(UtxoId[L1], Output[L1])] =
    for
        txId: TransactionHash <- genByteStringOfN(32).map(
            Hash.apply[Blake2b_256, HashPurpose.TransactionHash](_)
        )
        idx: Int <- Gen.choose(0, 1000)

        txIn = TransactionInput(
            transactionId = txId,
            index = idx
        )

        txOut = Babbage(
            address = TestPeer.address(peer),
            value = Value(Coin(1_000_000L)),
            datumOption = depositDatumFromPeer(peer),
            scriptRef = None
        )
    yield (UtxoId[L1](txIn), Output[L1](txOut))

/** Generate a semantically valid, but fully synthetic, nonsensical, genesis event coming from the
 * given peer
 */
def genL2EventGenesisFromPeer(peer: TestPeer): Gen[L2EventGenesis] = Gen.sized {
    numberOfDepositsAbsorbed =>
        // Always generate at least one deposit
        if numberOfDepositsAbsorbed == 0 then
            L2EventGenesis(Seq(genDepositFromPeer(peer).sample.get))
        else {
            var counter = 0
            var genesisSeq: Seq[(UtxoId[L1], Output[L1])] = Seq.empty
            while {
                val deposit = genDepositFromPeer(peer).sample.get
                if !(genesisSeq.contains(deposit))
                then {
                    genesisSeq = genesisSeq.appended(deposit);
                    counter = counter + 1
                }
                counter != numberOfDepositsAbsorbed
            } do ()
            L2EventGenesis(genesisSeq)
        }
}

/** Generate an "attack" that, given a context, state, and L2EventTransaction, returns a tuple
 * containing:
 *   - a mutated L2EventTransaction in such a way that a given ledger rule will be violated.
 *   - the expected error to be raised from the L2 ledger STS when the mutated transaction is
 *     applied.
 *
 * Note that, at this time, only one such attack can be applied at time; applying multiple attacks
 * and observing the exception would require using `Validation` rather than `Either`, and probably
 * some threading through of the various mutations to determine the actual context of the errors
 * raised.
 */
def genL2EventTransactionAttack: Gen[
    (Context, State, L2EventTransaction) => (L2EventTransaction, (String | TransactionException))
] = {

    // Violates "AllInputsMustBeInUtxoValidator" ledger rule
    def inputsNotInUtxoAttack: (Context, State, L2EventTransaction) => (
        L2EventTransaction,
            (String | TransactionException)
        ) =
        (context, state, transaction) => {
            // Generate a random TxId that is _not_ present in the state
            val bogusInputId: TransactionHash = Hash(
                genByteStringOfN(32)
                    .suchThat(txId =>
                        !state.utxo.toSeq.map(_._1.transactionId.bytes).contains(txId.bytes)
                    )
                    .sample
                    .get
            )

            val bogusTxIn = TransactionInput(transactionId = bogusInputId, index = 0)

            val newTx: L2EventTransaction = {
                val underlyingOriginal = transaction.transaction.untagged
                val underlyingModified = underlyingOriginal
                    // First focus on the inputs of the transaction
                    .focus(_.body.value.inputs)
                    // then modify those inputs: the goal is to replace the txId of one input with
                    // our bogusInputId
                    .modify(
                        // Inputs come as set, and I don't think monocle can `_.index(n)` a set,
                        // so we convert to and from List
                        _.toList
                            // Focus on the first element of the list, and...
                            .focus(_.index(0))
                            // replace its transactionId with our bogus txId
                            .replace(bogusTxIn)
                            .toSet
                    )

                L2EventTransaction(Tx[L2](underlyingModified))
            }

            val expectedException = new TransactionException.BadAllInputsUTxOException(
                transactionId = newTx.getEventId,
                missingInputs = Set(bogusTxIn),
                missingCollateralInputs = Set.empty,
                missingReferenceInputs = Set.empty
            )
            (newTx, expectedException)
        }

    Gen.oneOf(Seq(inputsNotInUtxoAttack))
}
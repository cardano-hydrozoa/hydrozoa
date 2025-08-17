package hydrozoa.l2.ledger

import hydrozoa.*
import hydrozoa.infra.transitionary.*
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.node.TestPeer.*
import hydrozoa.node.{
    TestPeer,
    l2EventTransactionFromInputsAndPeer,
    l2EventWithdrawalFromInputsAndPeer
}
import io.bullet.borer.Cbor
import monocle.Iso
import monocle.syntax.all.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Test as ScalaCheckTest}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.HashPurpose.KeyHash
import scalus.cardano.ledger.HashSize.given_HashSize_Blake2b_224
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State}
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.{v1, v3}
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

class HydrozoaMutatorSpec extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(200)
    }

    // Pre-compute and cache addresses
    address(Alice)
    address(Bob)

    test("init empty STS constituents") {
        val context = emptyContext
        val state = emptyState
        val event = L2EventTransaction(Tx[L2](emptyTransaction))
    }

    property("Random genesis event should succeed")(forAll(genL2EventGenesisFromPeer(Alice)) {
        (
            event =>
                HydrozoaL2Mutator(emptyContext, emptyState, event) match {
                    case Right(outState) =>
                        // Helper values used in properties and for logging error messages
                        val actualIndexes: Seq[Int] =
                            outState.utxo.map((txIn, _) => txIn.index).toSeq.sorted

                        (outState.certState == emptyState.certState) :|
                            "Genesis event should not modify CertState" &&
                            (outState.utxo.size == event.resolvedL2UTxOs.length) :|
                            "Genesis event should add correct number of UTxOs to the state" &&
                            (outState.utxo.forall((txIn, _) =>
                                txIn.transactionId == event.getEventId
                            )) :|
                            "All TransactionInputs resulting from the genesis should have the same txId" && {
                                // Checking that all expected indexes appear somewhere in the new state
                                var allIdx = Seq.range(0, event.resolvedL2UTxOs.length)
                                allIdx == actualIndexes
                            } :| s"All expected transaction indexes appear (0 to ${event.resolvedL2UTxOs.length}); actualIdxs are ${actualIndexes}"

                    case Left(err) => false :| (s"Genesis event failed with error: ${err}")
                }
        )
    })

    property("Alice can withdraw her own deposited utxos")(
      forAll(genL2EventGenesisFromPeer(Alice)) { event =>
          val outState =
              for
                  state <- HydrozoaL2Mutator(emptyContext, emptyState, event)
                  state <- HydrozoaL2Mutator(
                    emptyContext,
                    state,
                    l2EventWithdrawalFromInputsAndPeer(state.utxo.keySet, Alice)
                  )
              yield state
          outState match {
              case Left(err) => false :| (s"'genesis => withdraw all' failed with error: ${err}")
              case Right(outS) =>
                  (outS.certState == emptyState.certState) :| "'genesis => withdraw all' should not modify cert state" &&
                  outS.utxo.isEmpty :| "'genesis => withdraw all' should leave an empty utxo set"
          }
      }
    )

    property("Alice cannot withdraw Bob's genesis utxos")({
        forAll(genL2EventGenesisFromPeer(Bob)) { event =>
            // First apply the randomly generated genesis event with Bob's UTxOs
            // N.B.: we perform a partial pattern match, because we assume the validity of the above tests
            val Right(postGenesisState) =
                HydrozoaL2Mutator(emptyContext, emptyState, event): @unchecked
            // Then generate the withdrawal event, where Alice tries to withdrawal all UTxOs with her own key
            val withdrawlEvent = l2EventWithdrawalFromInputsAndPeer(
              postGenesisState.utxo.keySet,
              Alice
            )

            // Then attempt to execute the withdrawal
            val postWithdrawalState: Either[HydrozoaL2Mutator.Error, State] = HydrozoaL2Mutator(
              emptyContext,
              postGenesisState,
              withdrawlEvent
            )

            // We expect a failure _specifically_ due to missing signatures. Any other failures are rejected
            val expectedException = TransactionException.MissingKeyHashesException(
              transactionId = withdrawlEvent.getEventId,
              missingInputsKeyHashes = Set(
                Hash(ByteString.fromArray(address(Bob).payment.asHash.bytes))
              ),
              missingCollateralInputsKeyHashes = Set.empty,
              missingVotingProceduresKeyHashes = Set.empty,
              missingWithdrawalsKeyHashes = Set.empty,
              missingCertificatesKeyHashes = Set.empty,
              missingRequiredSignersKeyHashes = Set.empty
            )

            postWithdrawalState match {

                case Left(err) =>
                    err match {
                        case missingKeyHashes: TransactionException.MissingKeyHashesException =>
                            (missingKeyHashes == expectedException) :|
                                s"Correct exception type thrown (MissingKeyHashesException), but unexpected content. Actual: ${missingKeyHashes}; Expected: ${expectedException}"
                        case _ =>
                            false :| s"L2 STS failed for unexpected reason. Actual: ${err}; Expected: ${expectedException}"
                    }
                case Right(_) =>
                    false :| s"Alice was able to withdraw Bob's genesis UTxOs (i.e., Alice's signature validated on Bob's UTxOs)"
            }

        }
    })

    property("non-existent utxos can't be withdrawn")(
      forAll(genL2EventGenesisFromPeer(Alice)) { event =>
          // First apply the randomly generated genesis event with Alice's UTxOs
          // N.B.: we perform a partial pattern match, because we assume the validity of the above tests
          val Right(postGenesisState) =
              HydrozoaL2Mutator(emptyContext, emptyState, event): @unchecked

          // Then generate the withdrawal event, where Alice tries to withdrawal all UTxOs with her own key
          val allTxInputs = postGenesisState.utxo.keySet

          val withdrawalEvent = l2EventWithdrawalFromInputsAndPeer(
            allTxInputs,
            Alice
          )

          // Then attempt to execute the withdrawal on the _original empty state_
          val postWithdrawalState: Either[HydrozoaL2Mutator.Error, State] = HydrozoaL2Mutator(
            emptyContext,
            emptyState, // NOT postGenesisState
            withdrawalEvent
          )

          // We expect a failure _specifically_ due to missing inputs. Any other failures are rejected
          val expectedException = TransactionException.BadAllInputsUTxOException(
            transactionId = withdrawalEvent.transaction.id,
            missingInputs = allTxInputs,
            missingCollateralInputs = Set.empty,
            missingReferenceInputs = Set.empty
          )

          postWithdrawalState match {
              case Left(err) =>
                  (err == expectedException) :| s"Exceptions don't match. Actual: ${err.toString}; Expected: ${expectedException.toString}"
              case Right(_) =>
                  false :| s"Alice was able to withdraw non-existent utxos"
          }

      }
    )

    property("correct transaction")(forAll(genL2EventGenesisFromPeer(Alice)) { event =>
        val outState =
            for
                // First apply the randomly generated genesis event with Alice's UTxOs
                // N.B.: we perform a partial pattern match, because we assume the validity of the above tests
                postGenesisState <- HydrozoaL2Mutator(emptyContext, emptyState, event)

                // Then generate the transaction event, where Alice tries to send all UTxOs to bob
                allTxInputs = postGenesisState.utxo.keySet.map(UtxoId[L2](_))

                transactionEvent = l2EventTransactionFromInputsAndPeer(
                  inputs = allTxInputs,
                  utxoSet = (postGenesisState.utxo.unsafeAsL2),
                  inPeer = Alice,
                  outPeer = Bob
                )

                // Then attempt to execute the withdrawal on the postGenesisState
                finalState <- HydrozoaL2Mutator(
                  emptyContext,
                  postGenesisState,
                  transactionEvent
                )
            yield finalState

        outState match {
            case Left(err) => false :| s"Transaction failed with error: ${err}"
            case Right(outS) =>
                (outS.certState == emptyState.certState) :| "Successful transaction should not modify cert state" &&
                (outS.utxo.size == 1) :| "Successful transaction should result in a single utxo" &&
                (outS.utxo.head._2.address == TestPeer.address(
                  Bob
                )) :| "Result UTxO should be at Bob's address"
        }

    })

    property("transaction attack")(forAll(genL2EventGenesisFromPeer(Alice)) { event =>
        HydrozoaL2Mutator(emptyContext, emptyState, event) match {
            // This case should not happen (not the focus of the test), but we handle it anyway.
            case Left(genesisErr) => false :| s"Genesis event failed with error ${genesisErr}"
            case Right(postGenesisState) => {

                // Then generate the transaction event, where Alice tries to send all UTxOs to bob
                val allTxInputs = postGenesisState.utxo.keySet

                val transactionEvent = l2EventTransactionFromInputsAndPeer(
                  inputs = allTxInputs.map(UtxoId[L2](_)),
                  utxoSet = postGenesisState.utxo.unsafeAsL2,
                  inPeer = Alice,
                  outPeer = Bob
                )

                // Generate an attack
                val txAttack = genL2EventTransactionAttack.sample.get

                val (attackedTransaction, expectedError) =
                    txAttack(emptyContext, postGenesisState, transactionEvent)

                // Then attempt to execute the attacked transaction on the postGenesisState.
                // This should fail with the expected error.
                HydrozoaL2Mutator(emptyContext, postGenesisState, attackedTransaction) match {
                    case Left(actualErr) =>
                        (actualErr == expectedError) :| s"Actual error does not match expected error. Actual error: ${actualErr}. Expected error: ${expectedError}"
                    case Right(finalState) =>
                        false :| s"Expect a failure, but got a success. Final state: ${finalState}. Expected error ${expectedError}"
                }
            }
        }

    })
}

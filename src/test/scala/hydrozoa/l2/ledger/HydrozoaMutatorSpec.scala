package hydrozoa.l2.ledger

import hydrozoa.*
import hydrozoa.infra.transitionary.*
import hydrozoa.l2.ledger.generators.*
import hydrozoa.node.TestPeer.*
import hydrozoa.node.{
    TestPeer,
    l2EventTransactionFromInputsAndPeer,
    l2EventWithdrawalFromInputsAndPeer
}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Test as ScalaCheckTest
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.ledger.HashPurpose.KeyHash
import scalus.cardano.ledger.HashSize.given_HashSize_Blake2b_224
import scalus.cardano.ledger.rules.State

class HydrozoaMutatorSpec extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(200)
    }

    test("init empty STS constituents") {
        @annotation.unused
        val context = emptyContext
        @annotation.unused
        val state = emptyState
        @annotation.unused
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
                                val allIdx = Seq.range(0, event.resolvedL2UTxOs.length)
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

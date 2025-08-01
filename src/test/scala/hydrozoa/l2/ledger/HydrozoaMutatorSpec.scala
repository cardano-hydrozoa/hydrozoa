package hydrozoa.l2.ledger

import hydrozoa.infra.transitionary.*
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.node.TestPeer.*
import hydrozoa.node.{TestPeer, addressFromPeer, l2EventTransactionFromInputsAndPeer, l2EventWithdrawalFromInputsAndPeer}
import io.bullet.borer.Cbor
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Test as ScalaCheckTest}
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.HashPurpose.KeyHash
import scalus.cardano.ledger.HashSize.given_HashSize_Blake2b_224
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.State
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.{v1, v3}
import scalus.prelude.Option as SOption

// For these tests, we assume a 1:1:1 relationship between Peer:Wallet:Address.
// Note that this is NOT true in production.
val alice: TestPeer = TestPeer.Alice
val bob: TestPeer = TestPeer.Bob

/** Build dummy deposit datum from a pubkey, setting the L2 and refund addresses to the pkh address
  */
def depositDatumFromPeer(peer: TestPeer): Option[DatumOption] = {
    val pkh: AddrKeyHash = Hash(blake2b_224(ByteString.fromArray(account(peer).publicKeyBytes())))
    val v3Addr: v3.Address = v3
        .Address(
          credential = v3.Credential
              .PubKeyCredential(v3.PubKeyHash(pkh)),
          SOption.None
        )
    Some(
      Inline(
        toData(
          ByteString.fromArray(
            Cbor.encode(
              toData(
                DepositDatum(
                  address = v3Addr,
                  datum = SOption.None,
                  deadline = 100,
                  refundAddress = v3Addr,
                  refundDatum = SOption.None
                )
              )
            ).toByteArray
          )
        )
      )
    )

}

/** Generate a single, semantically valid but fully synthetic deposit for inclusion into a genesis
  * event
  */
def genDepositFromPeer(peer: TestPeer): Gen[(TransactionInput, TransactionOutput)] =
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
          address = addressFromPeer(peer),
          value = Value(Coin(1_000_000L)),
          datumOption = depositDatumFromPeer(peer),
          scriptRef = None
        )
    yield (txIn, txOut)

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
            var genesisSeq: Seq[(TransactionInput, TransactionOutput)] = Seq.empty
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

class HydrozoaMutatorSpec extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters = {
        // N.B.: 100 tests is insufficient coverage. When running in
        // CI or pre-release, it should be significantly higher.
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10)
    }

    test("init empty STS constituents") {
        val context = emptyContext
        val state = emptyState
        val event = L2EventTransaction(emptyTransaction)
    }

    property("Random genesis event should succeed")(forAll(genL2EventGenesisFromPeer(alice)) { event =>
        HydrozoaL2Mutator(emptyContext, emptyState, event) match {
            case Right(outState) =>
                // Helper values used in properties and for logging error messages
                val actualIndexs: Seq[Int] = outState.utxo.map((txIn, _) => txIn.index).toSeq.sorted

                (outState.certState == emptyState.certState) :|
                    "Genesis event should not modify CertState" &&
                    (outState.utxo.size == event.resolvedL2UTxOs.length) :|
                    "Genesis event should add correct number of UTxOs to the state" &&
                    (outState.utxo.forall((txIn, _) => txIn.transactionId == event.getEventId)) :|
                    "All TransactionInputs resulting from the genesis should have the same txId" && {
                        // Checking that all expected indexes appear somewhere in the new state
                        var allIdx = Seq.range(0, event.resolvedL2UTxOs.length)
                        allIdx == actualIndexs
                    } :| s"All expected transaction indexes appear (0 to ${event.resolvedL2UTxOs.length}); actualIdxs are ${actualIndexs}"

            case Left(err) => false :| (s"Genesis event failed with error: ${err}")
        }

    })

    property("Alice can withdraw her own deposited utxos")(
      forAll(genL2EventGenesisFromPeer(alice)) { event =>
          val outState =
              for
                  state <- HydrozoaL2Mutator(emptyContext, emptyState, event)
                  state <- HydrozoaL2Mutator(
                    emptyContext,
                    state,
                    l2EventWithdrawalFromInputsAndPeer(state.utxo.map(_._1).toSet, alice)
                  )
              yield state
          outState match {
              case Left(err) => false :| (s"'genesis => withdraw all' failed with error: ${err}")
              case Right(outS) =>
                  (outS.certState == emptyState.certState) :| "'genesis => withdraw all' should not modify cert state" &&
                  (outS.utxo.size == 0) :| "'genesis => withdraw all' should leave an empty utxo set"
          }
      }
    )

    property("Alice cannot withdraw Bob's genesis utxos")({
        forAll(genL2EventGenesisFromPeer(bob)) { event =>
            // First apply the randomly generated genesis event with Bob's UTxOs
            // N.B.: we perform a partial pattern match, because we assume the validity of the above tests
            val Right(postGenesisState) = HydrozoaL2Mutator(emptyContext, emptyState, event)
            // Then generate the withdrawal event, where Alice tries to withdrawal all UTxOs with her own key
            val withdrawlEvent = l2EventWithdrawalFromInputsAndPeer(
              postGenesisState.utxo.map(_._1).toSet,
              alice
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
                Hash[Blake2b_224, HashPurpose.KeyHash](
                  blake2b_224(
                    ByteString.fromArray(TestPeer.mkWallet(bob).exportVerificationKeyBytes.bytes)
                  )
                )
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
                            // So... Exception objects aren't comparable with ==. And it seems like I can't auto-derive
                            // === from cats because of this.
                            (missingKeyHashes.transactionId == expectedException.transactionId &&
                                missingKeyHashes.missingInputsKeyHashes == expectedException.missingInputsKeyHashes &&
                                missingKeyHashes.missingCertificatesKeyHashes == expectedException.missingCertificatesKeyHashes &&
                                missingKeyHashes.missingWithdrawalsKeyHashes == expectedException.missingWithdrawalsKeyHashes &&
                                missingKeyHashes.missingCollateralInputsKeyHashes == expectedException.missingCollateralInputsKeyHashes &&
                                missingKeyHashes.missingRequiredSignersKeyHashes == expectedException.missingRequiredSignersKeyHashes &&
                                missingKeyHashes.missingVotingProceduresKeyHashes == expectedException.missingVotingProceduresKeyHashes) :|
                                s"Correct exception type thrown (MissingKeyHashesException), but unexpected content. Actual: ${missingKeyHashes}; Expected: ${expectedException}"
                        case _ =>
                            false :| s"L2 STS failed for unexpected reason. Actual: ${err}; Expected: ${expectedException}"
                    }
                case Right(_) =>
                    false :| s"Alice was able to withdraw Bob's genesis UTxOs (i.e., Alice's signature validated on Bob's UTxOs) "
            }

        }
    })

    property("non-existent utxos can't be withdrawn")(
      forAll(genL2EventGenesisFromPeer(alice)) { event =>
          // First apply the randomly generated genesis event with Alice's UTxOs
          // N.B.: we perform a partial pattern match, because we assume the validity of the above tests
          val Right(postGenesisState) = HydrozoaL2Mutator(emptyContext, emptyState, event)

          // Then generate the withdrawal event, where Alice tries to withdrawal all UTxOs with her own key
          val allTxInputs = postGenesisState.utxo.map(_._1).toSet

          val withdrawalEvent = l2EventWithdrawalFromInputsAndPeer(
            allTxInputs,
            alice
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
                  (err.toString == expectedException.toString) :| s"Exception strings don't match. Actual: ${err.toString}; Expected: ${expectedException.toString}"
              case Right(_) =>
                  false :| s"Alice was able to withdraw non-existent utxos"
          }

      }
    )

    property("correct transaction")(forAll(genL2EventGenesisFromPeer(alice)) { event =>
        val outState =
            for
                // First apply the randomly generated genesis event with Alice's UTxOs
                // N.B.: we perform a partial pattern match, because we assume the validity of the above tests
                postGenesisState <- HydrozoaL2Mutator(emptyContext, emptyState, event)

                // Then generate the transaction event, where Alice tries to send all UTxOs to bob
                allTxInputs = postGenesisState.utxo.keySet

                transactionEvent = l2EventTransactionFromInputsAndPeer(
                  inputs = allTxInputs,
                  utxoSet = postGenesisState.utxo,
                  inPeer = alice,
                  outPeer = bob
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
                (outS.utxo.head._2.address == addressFromPeer(
                  bob
                )) :| "Result UTxO should be at Bob's address"
        }

    })
}

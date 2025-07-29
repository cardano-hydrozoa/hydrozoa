package hydrozoa.l2.ledger

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.transaction.spec.Transaction as BBTransaction
import hydrozoa.Wallet
import hydrozoa.infra.addWitness
import hydrozoa.infra.transitionary.*
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import io.bullet.borer.Cbor
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Test as ScalaCheckTest}
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.address.Address.Shelley
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.HashPurpose.KeyHash
import scalus.cardano.ledger.HashSize.{given_HashSize_Blake2b_224, given_HashSize_Blake2b_256}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.State
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN

import scala.collection.immutable.HashSet

/* TODO
Look at TestPeer, Wallet.scala, Tx.scala, and use the scalus/hydrozoa stuff to sign the transaction without round-tripping
through bloxbean
 */

// For these tests, we assume a 1:1:1 relationship between Peer:Wallet:Address.
// Note that this is NOT true in production.
val alice: TestPeer = TestPeer.Alice
val bob: TestPeer = TestPeer.Bob

def addressFromPeer(peer: TestPeer): Address = Shelley(
  ShelleyAddress(
    network = Mainnet,
    payment = Key(Hash(blake2b_224(ByteString.fromArray(account(peer).publicKeyBytes())))),
    delegation = Null
  )
)

// TODO: refactor all of this to make it just use the scalus types.
def signTx(peer: TestPeer, txUnsigned: Transaction): Transaction =
    val keyWitness = TestPeer.mkWallet(peer).createTxKeyWitness(txUnsigned.toHydrozoa)
    addWitness(txUnsigned.toHydrozoa, keyWitness).toScalus

/** Generate a single, semantically valid but fully synthetic deposit for inclusion into a genesis
  * event
  */
val genDeposit: Gen[(TransactionInput, TransactionOutput)] =
    for
        txId: TransactionHash <- genByteStringOfN(32).map(
          Hash.apply[Blake2b_256, HashPurpose.TransactionHash](_)
        )
        pkh: AddrKeyHash <- genByteStringOfN(28).map(Hash.apply[Blake2b_224, KeyHash](_))
        idx: Int <- Gen.choose(0, 1000)

        txIn = TransactionInput(
          transactionId = txId,
          index = idx
        )

        txOut = Babbage(
          address = Shelley(
            ShelleyAddress(
              network = Mainnet,
              payment = Key(pkh),
              delegation = Null
            )
          ),
          value = Value(Coin(1_000_000L)),
          datumOption = None,
          scriptRef = None
        )
    yield (txIn, txOut)

/** Generate a semantically valid, but fully synthetic, nonsensical, genesis event */
val genL2EventGenesis: Gen[L2EventGenesis] = Gen.sized { numberOfDepositsAbsorbed =>
    if numberOfDepositsAbsorbed == 0 then L2EventGenesis(Seq.empty)
    else {
        var counter = 0
        var genesisSeq: Seq[(TransactionInput, TransactionOutput)] = Seq.empty
        while {
            val deposit = genDeposit.sample.get
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

/** Generate a deposit from a specific test peer */
def genL2EventGenesisFromPeer(peer: TestPeer): Gen[L2EventGenesis] = {
    L2EventGenesis(genL2EventGenesis.sample.get.utxos.map((txIn, txOut) => {
        val babbage = txOut.asInstanceOf[Babbage]
        (txIn, babbage.copy(address = addressFromPeer(peer)))
    }))
}

/** Given a set of inputs event, construct a withdrawal event attempting to withdraw all inputs with
  * the given key
  */
def l2EventWithdrawalFromInputsAndPeer(
    inputs: Set[TransactionInput],
    peer: TestPeer
): L2EventWithdrawal = {
    val txBody: TransactionBody = TransactionBody(
      inputs = inputs,
      outputs = IndexedSeq.empty,
      fee = Coin(0L)
    )

    val txUnsigned: Transaction = {
        Transaction(
          body = KeepRaw(txBody),
          witnessSet = TransactionWitnessSet.empty,
          isValid = true,
          auxiliaryData = None
        )

    }

    // N.B.: round-tripping through bloxbean because this is the only way I know how to sign right now
    // Its probably possible to extract the key and use the crypto primitives from scalus directly
    L2EventWithdrawal(signTx(peer, txUnsigned))
}

/** Creates a pubkey transaction yielding a single UTxO from a set of inputs */
def l2EventTransactionFromInputsAndPeer(
    inputs: Set[TransactionInput],
    utxoSet: Map[TransactionInput, TransactionOutput],
    inPeer: TestPeer,
    outPeer: TestPeer
): L2EventTransaction = {

    val totalVal: Value = inputs.foldLeft(Value.zero)((v, ti) => v + utxoSet(ti).value)

    val txBody: TransactionBody = TransactionBody(
      inputs = inputs,
      outputs = IndexedSeq(
        Babbage(
          address = addressFromPeer(outPeer),
          value = totalVal,
          datumOption = None,
          scriptRef = None
        )
      ).map(Sized(_)),
      fee = Coin(0L)
    )

    val txUnsigned: Transaction = {
        Transaction(
          body = KeepRaw(txBody),
          witnessSet = TransactionWitnessSet.empty,
          isValid = false,
          auxiliaryData = None
        )
    }

    L2EventTransaction(signTx(inPeer, txUnsigned))
}

class HydrozoaMutatorSpec extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters = {
        // N.B.: 100 tests is insufficient coverage. When running in
        // CI or pre-release, it should be significantly higher.
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(100)
    }

    test("init empty STS constituents") {
        val context = emptyContext
        val state = emptyState
        val event = L2EventTransaction(emptyTransaction)
    }

    property("Random (nonsense) genesis event should succeed")(forAll(genL2EventGenesis) { event =>
        HydrozoaL2Mutator(emptyContext, emptyState, event) match {
            case Right(outState) =>
                // Helper values used in properties and for logging error messages
                val actualIndexs: Seq[Int] = outState.utxo.map((txIn, _) => txIn.index).toSeq.sorted

                (outState.certState == emptyState.certState) :|
                    "Genesis event should not modify CertState" &&
                    (outState.utxo.size == event.utxos.length) :|
                    "Genesis event should add correct number of UTxOs to the state" &&
                    (outState.utxo.forall((txIn, _) => txIn.transactionId == event.getEventId)) :|
                    "All TransactionInputs resulting from the genesis should have the same txId" && {
                        // Checking that all expected indexes appear somewhere in the new state
                        var allIdx = Seq.range(0, event.utxos.length)
                        allIdx == actualIndexs
                    } :| s"All expected transaction indexes appear (0 to ${event.utxos.length}); actualIdxs are ${actualIndexs}"

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
              missingInputsKeyHashes = HashSet(
                Hash[Blake2b_224, HashPurpose.KeyHash](
                  blake2b_224(ByteString.fromArray(TestPeer.mkWallet(bob).exportVerificationKeyBytes.bytes))
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
                            // So... Exception objects aren't comparable with ==. We just compare their messages
                            // instead here, I guess. The other option could be to write an extension method, but...
                            (missingKeyHashes.getMessage == expectedException.getMessage) :|
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
                allTxInputs = postGenesisState.utxo.map(_._1).toSet

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

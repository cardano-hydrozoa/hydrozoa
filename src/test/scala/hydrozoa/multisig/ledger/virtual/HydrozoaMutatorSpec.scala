package hydrozoa.multisig.ledger.virtual

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.multisig.ledger.VirtualLedger.{Config, State}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test as ScalaCheckTest}
import scalus.builtin.ByteString
import scalus.cardano.address.Network.Testnet
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v3
import scalus.prelude.Option as SOption
import test.Generators.Hydrozoa.{genL2EventTransactionAttack, genL2WithdrawalFromUtxosAndPeer}
import test.TestPeer.{Alice, Bob}
import test.{TestPeer, l2EventTransactionFromInputsAndPeer}

/** Build dummy deposit datum from a pubkey, setting the L2 and refund addresses to the pkh address
  */
def depositDatumFromPeer(peer: TestPeer): DepositUtxo.Datum = {
    val v3Addr: v3.Address = v3.Address(
      v3.Credential
          .PubKeyCredential(v3.PubKeyHash(peer.address(Testnet).payment.asHash)),
      SOption.None
    )

    DepositUtxo.Datum(
      address = v3Addr.credential,
      datum = SOption.None,
      deadline = 100,
      refundAddress = v3Addr,
      refundDatum = SOption.None
    )
}

/** Generate a single, semantically valid but fully synthetic deposit for inclusion into a genesis
  * event
  */
def genDepositFromPeer(peer: TestPeer, address: Option[ShelleyAddress] = None): Gen[DepositUtxo] =
    for
        txId: TransactionHash <- genByteStringOfN(32).map(
          Hash.apply[Blake2b_256, HashPurpose.TransactionHash](_)
        )
        idx: Int <- Gen.choose(0, 1000)

        txIn = TransactionInput(
          transactionId = txId,
          index = idx
        )

        coin <- Arbitrary.arbitrary[Coin]
    yield DepositUtxo(
      l1Input = txIn,
      l1OutputAddress = peer.address(Testnet),
      l1OutputDatum = depositDatumFromPeer(peer),
      l1OutputValue = coin,
      l1RefScript = None
    )

/** Generate a semantically valid, but fully synthetic, nonsensical, genesis event coming from the
  * given peer
  */
def genL2EventGenesisFromPeer(peer: TestPeer): Gen[L2EventGenesis] = Gen.sized {
    numberOfDepositsAbsorbed =>
        // Always generate at least one deposit
        if numberOfDepositsAbsorbed == 0 then {
            for {
                deposit <- genDepositFromPeer(peer)
            } yield L2EventGenesis.fromDepositUtxos(NonEmptyList.one(deposit))

        } else {
            var counter = 0
            var genesisSeq: Seq[DepositUtxo] = Seq.empty
            while {
                val deposit: DepositUtxo = genDepositFromPeer(peer).sample.get
                if !genesisSeq.contains(deposit)
                then {
                    genesisSeq = genesisSeq.appended(deposit);
                    counter = counter + 1
                }
                counter != numberOfDepositsAbsorbed
            } do ()
            L2EventGenesis.fromDepositUtxos(NonEmptyList.fromListUnsafe(genesisSeq.toList))
        }
}

object HydrozoaMutatorSpec extends Properties("Hydrozoa Mutator") {
    override def overrideParameters(p: ScalaCheckTest.Parameters): ScalaCheckTest.Parameters = {
        p.withMinSuccessfulTests(100)
    }

    // NOTE (Peter, 2025-12-03): This test is a bit of a hack, we need some stateful testing.
    // First we have to generate the L2 genesis event, then we have to execute it, then we have to generate the
    // withdrawal event and execute it.
    //
    // Scalacheck has a notion of stateful testing
    //    (see https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md#stateful-testing)
    // I _think_ this should work -- we just generate new commands based on the existing state.
    // But for now, for MVP purposes, its not worthwhile. Instead, we just sample the generator directly.
    val _ = property("Alice can withdraw her own deposited utxos") = {
        forAll(genL2EventGenesisFromPeer(Alice)) { genesisEvent =>
            val outState = {
                val genesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
                  genesisEvent.genesisObligations,
                  State.empty
                )

                for

                    withdrawal <- genL2WithdrawalFromUtxosAndPeer(
                      genesisState.activeUtxos,
                      Alice
                    ).sample.toRight("withdrawal failed to generate")
                    state <- HydrozoaTransactionMutator.transit(
                      Config.empty,
                      genesisState,
                      withdrawal
                    )
                yield state
            }
            outState match {
                case Left(err) => false :| s"'genesis => withdraw all' failed with error: $err"
                case Right(outS) =>
                    outS.activeUtxos.isEmpty :| "'genesis => withdraw all' should leave an empty utxo set"
            }
        }
    }

    val _ = property("Alice cannot withdraw Bob's genesis utxos") = {
        forAll(genL2EventGenesisFromPeer(Bob)) { genesisEvent =>
            val postGenesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
              genesisEvent.genesisObligations,
              State.empty
            )
            val res = for {
                // Then generate the withdrawal event, where Alice tries to withdrawal all UTxOs with her own key
                // FIXME (Peter, 2025-12-03): Again, this is more properly suited for stateful testing (see comment on
                // "Alice can withdraw her own deposited utxos test")
                withdrawlEvent <- genL2WithdrawalFromUtxosAndPeer(
                  postGenesisState.activeUtxos,
                  Alice
                ).sample.toRight("withdrawal failed to generate")

                // We expect a failure _specifically_ due to missing signatures. Any other failures are rejected
                expectedException = TransactionException.MissingKeyHashesException(
                  transactionId = withdrawlEvent.getEventId,
                  missingInputsKeyHashes = Set(
                    AddrKeyHash(ByteString.fromArray(Bob.address(Testnet).payment.asHash.bytes))
                  ),
                  missingCollateralInputsKeyHashes = Set.empty,
                  missingVotingProceduresKeyHashes = Set.empty,
                  missingWithdrawalsKeyHashes = Set.empty,
                  missingCertificatesKeyHashes = Set.empty,
                  missingRequiredSignersKeyHashes = Set.empty
                )

                // Then attempt to execute the withdrawal
                res <- HydrozoaTransactionMutator.transit(
                  Config.empty,
                  postGenesisState,
                  withdrawlEvent
                ) match {
                    // Check for exactly the error we expect.
                    case Left(err) =>
                        err match {
                            case missingKeyHashes: TransactionException.MissingKeyHashesException
                                if missingKeyHashes == expectedException =>
                                Right(())
                            case _ =>
                                Left(
                                  s"L2 STS failed for unexpected reason. Actual: $err; Expected: $expectedException"
                                )
                        }
                    case Right(_) =>
                        Left(
                          "Alice was able to withdraw Bob's genesis UTxOs (i.e., Alice's signature validated on Bob's UTxOs)"
                        )
                }
            } yield res
            res.isRight :| res.toString

        }
    }

    val _ = property("non-existent utxos can't be withdrawn") =
        forAll(genL2EventGenesisFromPeer(Alice)) { genesisEvent =>
            {
                val postGenesisState: State =
                    HydrozoaGenesisMutator.addGenesisUtxosToState(
                      genesisEvent.genesisObligations,
                      State.empty
                    )
                val res = for {
                    withdrawalEvent <- genL2WithdrawalFromUtxosAndPeer(
                      postGenesisState.activeUtxos,
                      Alice
                    ).sample.toRight("withdrawal failed to generate")

                    // We expect a failure _specifically_ due to missing inputs. Any other failures are rejected
                    expectedException = TransactionException.BadAllInputsUTxOException(
                      transactionId = withdrawalEvent.transaction.id,
                      missingInputs = postGenesisState.activeUtxos.keySet,
                      missingCollateralInputs = Set.empty,
                      missingReferenceInputs = Set.empty
                    )

                    res <- HydrozoaTransactionMutator.transit(
                      Config.empty,
                      State.empty,
                      withdrawalEvent
                    ) match {
                        case Left(err) if err != expectedException =>
                            Left(
                              s"Exceptions don't match. Actual: ${err.toString}; " +
                                  s"Expected: ${expectedException.toString}"
                            )
                        case Right(_) =>
                            Left("Alice was able to withdraw non-existent utxos")
                        case _ => Right(())
                    }
                } yield res
                res.isRight :| res.toString
            }
        }

    val _ = property("correct transaction") = forAll(genL2EventGenesisFromPeer(Alice)) {
        genesisEvent =>
            val postGenesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
              genesisEvent.genesisObligations,
              State.empty
            )

            // Then generate the transaction event, where Alice tries to send all UTxOs to bob
            val allTxInputs = TaggedSortedSet.from(postGenesisState.activeUtxos.keySet)

            val transactionEvent = l2EventTransactionFromInputsAndPeer(
              inputs = allTxInputs,
              utxoSet = postGenesisState.activeUtxos,
              inPeer = Alice,
              outPeer = Bob
            )

            // Then attempt to execute the withdrawal on the postGenesisState
            HydrozoaTransactionMutator.transit(
              Config.empty,
              postGenesisState,
              transactionEvent
            ) match {
                case Left(err) => false :| s"Transaction failed with error: $err"
                case Right(outS) =>
                    (outS.activeUtxos.size == 1) :| "Successful transaction should result in a single utxo" &&
                    (outS.activeUtxos.head._2.address == Bob
                        .address()) :| "Result UTxO should be at Bob's address"
            }

    }

    val _ = property("transaction attack") =
        forAll(genL2EventGenesisFromPeer(Alice), genL2EventTransactionAttack) {
            (genesisEvent, attack) =>
                val postGenesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
                  genesisEvent.genesisObligations,
                  State.empty
                )
                // Then generate the transaction event, where Alice tries to send all UTxOs to bob
                val allTxInputs = TaggedSortedSet.from(postGenesisState.activeUtxos.keySet)

                val transactionEvent = l2EventTransactionFromInputsAndPeer(
                  inputs = allTxInputs,
                  utxoSet = postGenesisState.activeUtxos,
                  inPeer = Alice,
                  outPeer = Bob
                )

                val (attackedTransaction, expectedError) =
                    attack(Config.empty, postGenesisState, transactionEvent)

                // Then attempt to execute the attacked transaction on the postGenesisState.
                // This should fail with the expected error.
                HydrozoaTransactionMutator.transit(
                  Config.empty,
                  postGenesisState,
                  attackedTransaction
                ) match {
                    case Left(actualErr) =>
                        (actualErr == expectedError) :| s"Actual error does not match expected error. Actual error: $actualErr. " +
                            s"Expected error: $expectedError"
                    case Right(finalState) =>
                        false :| s"Expect a failure, but got a success. Final state: $finalState. Expected error $expectedError"
                }
        }
}

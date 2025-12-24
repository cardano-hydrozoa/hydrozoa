//package hydrozoa.multisig.ledger.virtual
//
//import cats.FlatMap.nonInheritedOps.toFlatMapOps
//import cats.data.NonEmptyList
//import hydrozoa.*
//import hydrozoa.multisig.ledger.VirtualLedger.{Config, State}
//import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
//import org.scalacheck.Prop.{forAll, propBoolean}
//import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test as ScalaCheckTest}
//import scalus.builtin.ByteString
//import scalus.cardano.ledger.*
//import scalus.cardano.ledger.ArbitraryInstances.given
//import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
//import scalus.ledger.api.v3
//import scalus.prelude.Option as SOption
//import test.Generators.Hydrozoa.{genGenesisObligation, genL2EventTransactionAttack, genL2WithdrawalFromUtxosAndPeer}
//import test.TestPeer.{Alice, Bob}
//import test.{TestPeer, l2EventTransactionFromInputsAndPeer, testNetwork, testVirtualLedgerConfig}
//
///** Build dummy deposit datum from a pubkey, setting the L2 and refund addresses to the pkh address
//  */
//def depositDatumFromPeer(peer: TestPeer): DepositUtxo.Datum = {
//    val v3Addr: v3.Address = v3.Address(
//      v3.Credential
//          .PubKeyCredential(v3.PubKeyHash(peer.address(testNetwork).payment.asHash)),
//      SOption.None
//    )
//
//    DepositUtxo.Datum(
//      DepositUtxo.Refund.Instructions(
//        address = v3Addr,
//        datum = SOption.None,
//        startTime = 100,
//      )
//    )
//}
//
///** Generate a single, semantically valid but fully synthetic deposit for inclusion into a genesis
//  * event
//  */
//def genDepositUtxoFromPeer(
//    peer: TestPeer,
//): Gen[DepositUtxo] =
//    for
//        txId: TransactionHash <- genByteStringOfN(32).map(
//          Hash.apply[Blake2b_256, HashPurpose.TransactionHash](_)
//        )
//        idx: Int <- Gen.choose(0, 1000)
//
//        txIn = TransactionInput(
//          transactionId = txId,
//          index = idx
//        )
//
//        coin <- Arbitrary.arbitrary[Coin]
//
//        virtualOutputs <- Gen
//            .nonEmptyListOf(genGenesisObligation(peer))
//            .map(NonEmptyList.fromListUnsafe)
//    yield DepositUtxo(
//      l1Input = txIn,
//      l1OutputAddress = peer.address(testNetwork),
//      l1OutputDatum = depositDatumFromPeer(peer),
//      l1OutputValue = Value(coin),
//      virtualOutputs = virtualOutputs
//    )
//
///** Generate a semantically valid, but fully synthetic, nonsensical, genesis event coming from the
//  * given peer
//  */
//def genL2EventGenesisFromPeer(peer: TestPeer): Gen[L2EventGenesis] =
//    for {
//        numberOfGenesisObligations <- Gen.choose[Int](1, 50)
//        gos <- Gen
//            .listOfN(numberOfGenesisObligations, genGenesisObligation(peer))
//            .map(l => NonEmptyList.fromListUnsafe(l.distinct))
//        gid <- Arbitrary.arbitrary[TransactionHash]
//    } yield L2EventGenesis(gos, gid)
//
//object HydrozoaMutatorSpec extends Properties("Hydrozoa Mutator") {
//    override def overrideParameters(p: ScalaCheckTest.Parameters): ScalaCheckTest.Parameters = {
//        p.withMinSuccessfulTests(100)
//    }
//
//    import org.scalacheck.PropertyM.*
//
//    // TODO: Factor this out? Maybe make these as implicits?
//    type EA[A] = Either[Any, A]
//
//    def runner: EA[Prop] => Prop = {
//        case Left(err)   => false :| s"test failed with error: $err"
//        case Right(prop) => prop
//    }
//
//    val _ = property("Alice can withdraw her own deposited utxos") = {
//        import org.scalacheck.PropertyM
//        import org.scalacheck.PropertyM.*
//
//        type EA[A] = Either[Any, A]
//
//        def runner: EA[Prop] => Prop = {
//            case Left(err)   => false :| s"test failed with error: $err"
//            case Right(prop) => prop
//        }
//
//        monadic(
//          runner,
//          m = for {
//              genesisEvent <- pick[EA, L2EventGenesis](genL2EventGenesisFromPeer(Alice))
//              genesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
//                genesisEvent,
//                State.empty
//              )
//              withdrawal <- pick[EA, L2EventTransaction](
//                genL2WithdrawalFromUtxosAndPeer(genesisState.activeUtxos, Alice)
//              )
//
//              // FIXME: This throws away all testcases that are too big. We should adjust the generator to avoid that
//              vlConfig = testVirtualLedgerConfig(0)
//              _ <- pre[EA](withdrawal.transaction.toCbor.length < vlConfig.protocolParams.maxTxSize)
//              state <- run[EA, State](
//                HydrozoaTransactionMutator.transit(vlConfig, genesisState, withdrawal)
//              )
//              _ <- assertWith(
//                msg = "'genesis => withdraw all' should leave empty utxo set",
//                condition = state.activeUtxos.isEmpty
//              )
//          } yield true
//        )
//
//    }
//
//    val _ = property("Alice cannot withdraw Bob's genesis utxos") = {
//        forAll(genL2EventGenesisFromPeer(Bob)) { genesisEvent =>
//            val postGenesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
//              genesisEvent,
//              State.empty
//            )
//            val res = for {
//                // Then generate the withdrawal event, where Alice tries to withdrawal all UTxOs with her own key
//                // FIXME (Peter, 2025-12-03): Again, this is more properly suited for stateful testing (see comment on
//                // "Alice can withdraw her own deposited utxos test")
//                withdrawlEvent <- genL2WithdrawalFromUtxosAndPeer(
//                  postGenesisState.activeUtxos,
//                  Alice
//                ).sample.toRight("withdrawal failed to generate")
//
//                // We expect a failure _specifically_ due to missing signatures. Any other failures are rejected
//                expectedException = TransactionException.MissingKeyHashesException(
//                  transactionId = withdrawlEvent.transaction.id,
//                  missingInputsKeyHashes = Set(
//                    AddrKeyHash(ByteString.fromArray(Bob.address(testNetwork).payment.asHash.bytes))
//                  ),
//                  missingCollateralInputsKeyHashes = Set.empty,
//                  missingVotingProceduresKeyHashes = Set.empty,
//                  missingWithdrawalsKeyHashes = Set.empty,
//                  missingCertificatesKeyHashes = Set.empty,
//                  missingRequiredSignersKeyHashes = Set.empty
//                )
//
//                // Then attempt to execute the withdrawal
//                res <- HydrozoaTransactionMutator.transit(
//                  Config.empty,
//                  postGenesisState,
//                  withdrawlEvent
//                ) match {
//                    // Check for exactly the error we expect.
//                    case Left(err) =>
//                        err match {
//                            case missingKeyHashes: TransactionException.MissingKeyHashesException
//                                if missingKeyHashes == expectedException =>
//                                Right(())
//                            case _ =>
//                                Left(
//                                  s"L2 STS failed for unexpected reason. Actual: $err; Expected: $expectedException"
//                                )
//                        }
//                    case Right(_) =>
//                        Left(
//                          "Alice was able to withdraw Bob's genesis UTxOs (i.e., Alice's signature validated on Bob's UTxOs)"
//                        )
//                }
//            } yield res
//            res.isRight :| res.toString
//
//        }
//    }
//
//    val _ = property("non-existent utxos can't be withdrawn") =
//        forAll(genL2EventGenesisFromPeer(Alice)) { genesisEvent =>
//            {
//                val postGenesisState: State =
//                    HydrozoaGenesisMutator.addGenesisUtxosToState(
//                      genesisEvent,
//                      State.empty
//                    )
//                val res = for {
//                    withdrawalEvent <- genL2WithdrawalFromUtxosAndPeer(
//                      postGenesisState.activeUtxos,
//                      Alice
//                    ).sample.toRight("withdrawal failed to generate")
//
//                    // We expect a failure _specifically_ due to missing inputs. Any other failures are rejected
//                    expectedException = TransactionException.BadAllInputsUTxOException(
//                      transactionId = withdrawalEvent.transaction.id,
//                      missingInputs = postGenesisState.activeUtxos.keySet,
//                      missingCollateralInputs = Set.empty,
//                      missingReferenceInputs = Set.empty
//                    )
//
//                    res <- HydrozoaTransactionMutator.transit(
//                      Config.empty,
//                      State.empty,
//                      withdrawalEvent
//                    ) match {
//                        case Left(err) if err != expectedException =>
//                            Left(
//                              s"Exceptions don't match. Actual: ${err.toString}; " +
//                                  s"Expected: ${expectedException.toString}"
//                            )
//                        case Right(_) =>
//                            Left("Alice was able to withdraw non-existent utxos")
//                        case _ => Right(())
//                    }
//                } yield res
//                res.isRight :| res.toString
//            }
//        }
//
//    val _ = property("correct transaction") = {
//
//        monadic(
//          runner,
//          for {
//              genesisEvent <- pick[EA, L2EventGenesis](genL2EventGenesisFromPeer(Alice))
//              postGenesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
//                genesisEvent,
//                State.empty
//              )
//              allTxInputs = TaggedSortedSet.from(postGenesisState.activeUtxos.keySet)
//              transactionEvent = l2EventTransactionFromInputsAndPeer(
//                inputs = allTxInputs,
//                utxoSet = postGenesisState.activeUtxos,
//                inPeer = Alice,
//                outPeer = Bob
//              )
//              vlConfig = testVirtualLedgerConfig(0)
//
//              // Discard all test cases where the tx size would be too big.
//              // FIXME: We should just generate smaller things
//              _ <- pre[EA](
//                transactionEvent.transaction.toCbor.length < vlConfig.protocolParams.maxTxSize
//              )
//
//              state <- run[EA, State](
//                HydrozoaTransactionMutator.transit(vlConfig, postGenesisState, transactionEvent)
//              )
//
//              _ <- assertWith[EA](
//                state.activeUtxos.size == 1,
//                "Successful transaction should result in a single utxo"
//              )
//              _ <- assertWith[EA](
//                state.activeUtxos.head._2.address == Bob.address(),
//                "Result UTxO should be at Bob's address"
//              )
//          } yield true
//        )
//    }
//
//    val _ = property("transaction attack") =
//        forAll(genL2EventGenesisFromPeer(Alice), genL2EventTransactionAttack) {
//            (genesisEvent, attack) =>
//                val postGenesisState = HydrozoaGenesisMutator.addGenesisUtxosToState(
//                  genesisEvent,
//                  State.empty
//                )
//                // Then generate the transaction event, where Alice tries to send all UTxOs to bob
//                val allTxInputs = TaggedSortedSet.from(postGenesisState.activeUtxos.keySet)
//
//                val transactionEvent = l2EventTransactionFromInputsAndPeer(
//                  inputs = allTxInputs,
//                  utxoSet = postGenesisState.activeUtxos,
//                  inPeer = Alice,
//                  outPeer = Bob
//                )
//
//                val (attackedTransaction, expectedError) =
//                    attack(Config.empty, postGenesisState, transactionEvent)
//
//                // Then attempt to execute the attacked transaction on the postGenesisState.
//                // This should fail with the expected error.
//                HydrozoaTransactionMutator.transit(
//                  Config.empty,
//                  postGenesisState,
//                  attackedTransaction
//                ) match {
//                    case Left(actualErr) =>
//                        (actualErr == expectedError) :| s"Actual error does not match expected error. Actual error: $actualErr. " +
//                            s"Expected error: $expectedError"
//                    case Right(finalState) =>
//                        false :| s"Expect a failure, but got a success. Final state: $finalState. Expected error $expectedError"
//                }
//        }
//}

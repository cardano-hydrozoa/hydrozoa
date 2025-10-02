package hydrozoa.lib.tx

import hydrozoa.lib.tx.*
import hydrozoa.lib.tx.CredentialWitness.PlutusScriptCredential
import hydrozoa.lib.tx.ExpectedWitnessType.ScriptHashWitness
import hydrozoa.lib.tx.ReferenceDataAction.{ReferenceInput, SpendInput}
import hydrozoa.lib.tx.OutputWitness.{NativeScriptOutput, PlutusScriptOutput}
import hydrozoa.lib.tx.RedeemerPurpose.{ForCert, ForMint}
import hydrozoa.lib.tx.ScriptWitness.ScriptValue
import hydrozoa.lib.tx.TransactionBuilder.{Context, build, modify}
import hydrozoa.lib.tx.TransactionBuilderStep.*
import hydrozoa.lib.tx.TransactionEditor.{editTransaction, editTransactionSafe}
import hydrozoa.lib.tx.TxBuildError.{
    IncorrectScriptHash,
    UnneededDeregisterWitness,
    WrongNetworkId,
    WrongOutputType
}
import hydrozoa.{emptyTransaction, txBodyL}
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import org.scalacheck.Gen
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.ShelleyDelegationPart.{Key, Null}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Hash.given
import scalus.cardano.ledger.Certificate.UnregCert
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.RedeemerTag.{Cert, Spend}
import scalus.cardano.ledger.Timelock.AllOf
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.|>
import test.*
import test.TestPeer.Alice

import scala.collection.immutable.{SortedMap, SortedSet}

private def addInput(input: TransactionInput): Transaction => Transaction =
    txBodyL
        .refocus(_.inputs)
        .modify((is: TaggedOrderedSet[TransactionInput]) =>
            TaggedOrderedSet.from(
              is.toSortedSet + input
            )
        )

class TxBuilderTest extends munit.ScalaCheckSuite {
    /*
          testBuilderStepsFail
              :: String
              -> Array TransactionBuilderStep
              -> TxBuildError
              -> TestPlanM (Aff Unit) Unit
              testBuilderStepsFail label steps err = test label do
              let
              result = buildTransaction steps
                  result `shouldEqual` Left err
     */

    /** Test that the builder steps fail with the expected error
      *
      * @param label
      * @param steps
      * @param error
      */
    def testBuilderStepsFail(
        label: String,
        steps: Seq[TransactionBuilderStep],
        error: TxBuildError
    )(implicit loc: munit.Location): Unit =
        test(label)({
            val res = TransactionBuilder.build(Mainnet, steps)
            assertEquals(obtained = res, expected = Left(error))
        })

    /*
        testBuilderSteps
            :: String
            -> Array TransactionBuilderStep
            -> Transaction
            -> TestPlanM (Aff Unit) Unit
            testBuilderSteps label steps expected = test label do
            let
            result = buildTransaction steps
                result `shouldEqual` Right expected
     */
    def testBuilderSteps(
        label: String,
        steps: Seq[TransactionBuilderStep],
        expected: (
            Transaction,
            Seq[DetachedRedeemer],
            Network,
            Set[ExpectedSigner],
            Set[TransactionUnspentOutput]
        )
    )(implicit loc: munit.Location): Unit =
        test(label)({
            val res = TransactionBuilder.build(Mainnet, steps)
            assertEquals(
              obtained = res.map(_.toTuple),
              expected = Right(expected)
            )
        })

    /*
        pkhUtxo =
             TransactionUnspentOutput
              { input: input1
             , output: pkhOutput
              }
     */
    val pkhUtxo = TransactionUnspentOutput(input = input1, output = pkhOutput)
    /*
        skhUtxo =
            TransactionUnspentOutput
            { input: input1
            , output: skhOutput
            }
     */
    val skhUtxo = TransactionUnspentOutput(input1, skhOutput)

    val ns: Script.Native = Script.Native(AllOf(IndexedSeq.empty))
    val nsSigners: Set[AddrKeyHash] = Gen.listOf(genAddrKeyHash).sample.get.toSet
    val nsWitness = NativeScriptOutput(ScriptValue(ns, nsSigners))

    val script2Signers: Set[AddrKeyHash] = Gen.listOf(genAddrKeyHash).sample.get.toSet
    val plutusScript2Witness =
        PlutusScriptOutput(ScriptValue(script2, script2Signers), Data.List(List()), None)

    private def setScriptAddr(
        scriptHash: ScriptHash,
        utxo: (TransactionInput, Babbage)
    ): (TransactionInput, Babbage) =
        utxo.focus(_._2.address)
            .replace(
              ShelleyAddress(
                network = Mainnet,
                payment = ShelleyPaymentPart.Script(scriptHash),
                delegation = Null
              )
            )

    // A Utxo at the address for script 1
    val script1Utxo: TransactionUnspentOutput = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get

        TransactionUnspentOutput(setScriptAddr(script1.scriptHash, utxo))
    }

    // A Utxo at the address for script 2
    val script2Utxo: TransactionUnspentOutput = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        TransactionUnspentOutput(setScriptAddr(script2.scriptHash, utxo))
    }

    // Expected Signers for the plutus script1 ref witness
    val psRefWitnessExpectedSigners: Set[AddrKeyHash] = Gen.listOf(genAddrKeyHash).sample.get.toSet

    private def setRefScript(
        script: Script,
        utxo: (TransactionInput, Babbage)
    ): (TransactionInput, Babbage) =
        utxo.focus(_._2.scriptRef).replace(Some(ScriptRef(script)))

    // A utxo carrying a reference script for script 1
    val utxoWithScript1ReferenceScript: TransactionUnspentOutput = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        TransactionUnspentOutput(setRefScript(script1, utxo))
    }

    // A utxo carrying a reference script for script 2
    val utxoWithScript2ReferenceScript: TransactionUnspentOutput = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        TransactionUnspentOutput(setRefScript(script2, utxo))
    }

    //    plutusScriptRefWitness =
    //      PlutusScriptOutput (ScriptReference input1 SpendInput) RedeemerDatum.unit
    //        Nothing
    val plutusScript1RefWitness = PlutusScriptOutput(
      ScriptWitness
          .ScriptReference(utxoWithScript1ReferenceScript, SpendInput, psRefWitnessExpectedSigners),
      Data.List(List()),
      None
    )

    val plutusScript2RefWitness = PlutusScriptOutput(
      ScriptWitness.ScriptReference(
        utxoWithScript2ReferenceScript,
        ReferenceInput,
        psRefWitnessExpectedSigners
      ),
      Data.List(List()),
      None
    )

    ///////////////////////////////////////////////////////////////
    // Group: "SpendOutput"
    ///////////////////////////////////////////////////////////////

    val spendPkhUtxoStep = TransactionBuilderStep.SpendOutput(pkhUtxo, None)
    val pubKeyInput1Expected: ContextTuple =
        Context.empty(Mainnet).toTuple
            |> transactionL
                .andThen(txBodyL.refocus(_.inputs))
                .replace(TaggedOrderedSet(input1))
            |> expectedSignersL
                .modify(_ ++ fromRight(spendPkhUtxoStep.additionalSigners))
            |> resolvedUtxosL.modify(_ + pkhUtxo)

    // testBuilderSteps "PKH output" [ SpendOutput pkhUtxo Nothing ] $
    //      anyNetworkTx # _body <<< _inputs .~ [ input1 ]
    testBuilderSteps(
      label = "PKH Output",
      steps = List(SpendOutput(pkhUtxo, None)),
      expected = pubKeyInput1Expected
    )

    // testBuilderSteps "PKH output x2 -> 1"
    //      [ SpendOutput pkhUtxo Nothing, SpendOutput pkhUtxo Nothing ] $
    //      anyNetworkTx # _body <<< _inputs .~ [ input1 ]
    testBuilderSteps(
      label = "PKH output x2 -> 1",
      steps = List(SpendOutput(pkhUtxo, None), SpendOutput(pkhUtxo, None)),
      expected = pubKeyInput1Expected
    )

    // testBuilderStepsFail "PKH output with wrong witness #1"
    //      [ SpendOutput pkhUtxo (Just nsWitness) ] $
    //      WrongOutputType (ScriptHashWitness nsWitness) pkhUtxo
    testBuilderStepsFail(
      label = "PKH output with wrong witness #1",
      steps = List(SpendOutput(pkhUtxo, Some(nsWitness))),
      error = WrongOutputType(ScriptHashWitness(nsWitness), pkhUtxo)
    )

    //     testBuilderStepsFail "PKH output with wrong witness #2"
    //      [ SpendOutput pkhUtxo (Just plutusScriptRefWitness) ] $
    //      WrongOutputType (ScriptHashWitness plutusScriptRefWitness) pkhUtxo
    testBuilderStepsFail(
      label = "PKH output with wrong witness #2",
      steps = List(SpendOutput(pkhUtxo, Some(plutusScript1RefWitness))),
      error = WrongOutputType(ScriptHashWitness(plutusScript1RefWitness), pkhUtxo)
    )

    //  testBuilderStepsFail "SKH output with wrong witness #1"
    //      [ SpendOutput skhUtxo (Just nsWitness) ] $
    //      IncorrectScriptHash (Left ns) scriptHash1
    testBuilderStepsFail(
      label = "SKH output with wrong witness #1",
      steps = List(SpendOutput(skhUtxo, Some(nsWitness))),
      error = IncorrectScriptHash(Left(ns), scriptHash1)
    )

    // testBuilderStepsFail "SKH output with wrong witness #2"
    //    [ SpendOutput skhUtxo (Just plutusScriptWitness) ] $
    //    IncorrectScriptHash (Right script2) scriptHash1
    testBuilderStepsFail(
      label = "SKH output with wrong witness #2",
      steps = List(SpendOutput(skhUtxo, Some(plutusScript2Witness))),
      error = IncorrectScriptHash(Right(script2), scriptHash1)
    )

    //     test "PKH output with wrong NetworkId" do
    //      let
    //        result =
    //          modifyTransaction testnetTransaction
    //            [ SpendOutput pkhUtxo Nothing ]
    //      result `shouldEqual`
    //        Left (WrongNetworkId $ pkhUtxo ^. _output <<< _address)
    test("PKH output with wrong NetworkId")({
        val pkhUtxoTestNet =
            TransactionUnspentOutput(
              input = input0,
              output = Babbage(
                address = ShelleyAddress(
                  network = Testnet,
                  payment = pkhOutputPaymentPart,
                  delegation = Null
                ),
                value = Value.zero,
                datumOption = None,
                scriptRef = None
              )
            )
        val res = modify(
          Context.empty(Mainnet),
          List(SpendOutput(pkhUtxo, None), SpendOutput(pkhUtxoTestNet, None))
        ) // Mainnet context with mixed network addresses
        assertEquals(obtained = res, expected = Left(WrongNetworkId(pkhUtxoTestNet.output.address)))
    })

    //// @Ilia: This should build successfully: we are spending a utxo located at the address for script one,
    //// and providing a reference utxo carrying the same script in its scriptRef
    // testBuilderSteps(
    //  label = "Script Output with Script Ref Withness",
    //  steps = List(SpendOutput(utxo = script1Utxo, witness = Some(plutusScript1RefWitness))),
    //  expected = ???
    // )

    //// @Ilia: This should currently be failing. script1Utxo is utxo at the script1 address, while the witness passed
    //// denotes a utxo carrying script2 in its scriptRef.
    //// Since we just added this, I don't know if we currently have an appropriate error constructor yet.
    // testBuilderStepsFail(
    //  label = "PKH Output with mismatched script ref",
    //  steps = List(SpendOutput(utxo = script1Utxo, witness = Some(plutusScript2RefWitness))),
    //  error = ???
    // )

    // @Ilia: this should pass. I've added it to usePlutusScriptWitness.
    // A similar test could be written for native scripts
    test("Referencing a script Utxo adds the utxo to context's resolvedUtxos") {
        val steps = List(SpendOutput(utxo = script1Utxo, witness = Some(plutusScript1RefWitness)))
        val built = fromRight(TransactionBuilder.build(Mainnet, steps))
        assertEquals(
          obtained = built.toTuple |> resolvedUtxosL.get,
          Set(script1Utxo, utxoWithScript1ReferenceScript)
        )
    }

    // ================================================================
    // Subgroup: Signature tracking
    // ================================================================

    test("SpendOutput.additionalSignersUnsafe works for pubkey")(
      {

          // Check that the additional signers are what we expect
          assertEquals(
            obtained = spendPkhUtxoStep.additionalSigners,
            expected = Right(
              Set(
                ExpectedSigner(
                  pkhOutputPaymentPart.asInstanceOf[ShelleyPaymentPart.Key].hash
                )
              )
            )
          )

          // Check that the transaction step adds the correct signer
          val tx = build(Mainnet, List(spendPkhUtxoStep))
          assertEquals(
            obtained = tx.map(_.expectedSigners),
            expected = Right(fromRight(spendPkhUtxoStep.additionalSigners))
          )

      }
    )

    test("Signers works for NS spend")({
        val txInput = genTransactionInput.sample.get

        val step =
            TransactionBuilderStep.SpendOutput(
              utxo = TransactionUnspentOutput(
                txInput,
                Babbage(
                  address = ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(ns.scriptHash), Null),
                  value = Value.zero,
                  datumOption = None,
                  scriptRef = None
                )
              ),
              witness = Some(nsWitness)
            )

        // Signers are what we expected for an NS spend
        assertEquals(
          obtained = step.additionalSigners,
          expected = Right(nsSigners.map(ExpectedSigner(_)))
        )

        // Signers are what we expect for a transaction built with this step
        assertEquals(
          obtained = build(Mainnet, List(step)).map(_.expectedSigners),
          expected = Right(fromRight(step.additionalSigners))
        )
    })

    test("Signers work for PS spend")({
        val txInput = genTransactionInput.sample.get
        val step =
            TransactionBuilderStep.SpendOutput(
              utxo = TransactionUnspentOutput(
                txInput,
                Babbage(
                  address =
                      ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(script2.scriptHash), Null),
                  value = Value.zero,
                  datumOption = Some(Inline(Data.List(List.empty))),
                  scriptRef = None
                )
              ),
              witness = Some(plutusScript2Witness)
            )

        // Signers are what we expected for an PS spend
        assertEquals(
          obtained = fromRight(step.additionalSigners),
          expected = script2Signers.map(ExpectedSigner(_))
        )

        val built = fromRight(build(Mainnet, List(step)))

        // Signers are what we expect for a transaction built with this step
        assertEquals(
          obtained = built.expectedSigners,
          expected = fromRight(step.additionalSigners)
        )

        // signers are added to the `requiredSigners` field in tx body
        assertEquals(
          obtained = built.toTuple |> transactionL.andThen(txBodyL).refocus(_.requiredSigners).get,
          expected = TaggedOrderedSet.from(fromRight(step.additionalSigners).map(_.hash))
        )
    })

    // =======================================================================
    // Group: "Pay"
    // =======================================================================

    //     testBuilderSteps "#1" [ Pay pkhOutput ] $
    //      anyNetworkTx # _body <<< _outputs .~ [ pkhOutput ]
    testBuilderSteps(
      label = "Pay #1",
      steps = List(Pay(pkhOutput)),
      expected = Context.empty(Mainnet).toTuple
          |> transactionL
              .andThen(txBodyL.refocus(_.outputs))
              .replace(IndexedSeq(Sized(pkhOutput)))
    )

    // =======================================================================
    // Group: "MintAsset"
    // =======================================================================

    //     testBuilderSteps "#1" [ Pay pkhOutput ] $
    //      anyNetworkTx # _body <<< _outputs .~ [ pkhOutput ]
    // NOTE (dragospe, 2025-09-24): upstream, this test is the same as pay #1. I've modified it.
    testBuilderSteps(
      label = "MintAsset #1",
      steps = List(
        MintAsset(
          scriptHash = scriptHash1,
          assetName = AssetName(ByteString.fromHex("deadbeef")),
          amount = 1L,
          witness = CredentialWitness.PlutusScriptCredential(
            ScriptWitness.ScriptValue(script1, Set.empty),
            redeemer = Data.List(List.empty)
          )
        )
      ),
      expected = Context.empty(Mainnet).toTuple |>
          // replace mint
          transactionL
              .andThen(txBodyL)
              .refocus(_.mint)
              .replace(
                Some(
                  Mint(
                    MultiAsset(
                      SortedMap.from(
                        List(
                          scriptHash1 -> SortedMap.from(
                            List(AssetName(ByteString.fromHex("deadbeef")) -> 1L)
                          )
                        )
                      )
                    )
                  )
                )
              )
          |>
          // add script witness
          transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .replace(Set(script1))
          |>
          // add redeemer
          transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(
                Some(
                  KeepRaw(
                    Redeemers(
                      Redeemer(
                        tag = RedeemerTag.Mint,
                        index = 0,
                        data = Data.List(List.empty),
                        exUnits = ExUnits.zero
                      )
                    )
                  )
                )
              )
          |>
          redeemersL.replace(
            List(
              DetachedRedeemer(
                datum = Data.List(List.empty),
                purpose = ForMint(
                  ScriptHash.fromHex("36137e3d612d23a644283f10585958085aa255bdae4076fcefe414b6")
                )
              )
            )
          )
    )

    // =======================================================================
    // Group: "Deregister"
    // =======================================================================

    // testBuilderSteps "Deregister script"
    //      [ IssueCertificate
    //          ( StakeDeregistration
    //              (wrap $ ScriptHashCredential $ PlutusScript.hash script1)
    //          )
    //          $ Just
    //          $ PlutusScriptCredential (ScriptValue script1) RedeemerDatum.unit
    //      ] $
    //      anyNetworkTx
    //        # _witnessSet <<< _plutusScripts .~ [ script1 ]
    //        # _witnessSet <<< _redeemers .~
    //            [ Redeemer
    //                { exUnits: ExUnits.empty
    //                , tag: Cert
    //                , index: BigNum.zero
    //                , data: RedeemerDatum.unit
    //                }
    //            ]
    //        # _body <<< _certs .~
    //            [ StakeDeregistration $ wrap $ ScriptHashCredential $
    //                PlutusScript.hash script1
    //            ]
    testBuilderSteps(
      label = "Deregister script",
      steps = List(
        IssueCertificate(
          cert = Certificate.UnregCert(Credential.ScriptHash(script1.scriptHash), coin = None),
          witness = Some(
            PlutusScriptCredential(
              ScriptWitness.ScriptValue(script1, Set.empty),
              Data.List(List.empty)
            )
          )
        )
      ),
      expected = Context.empty(Mainnet).toTuple |>
          transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .replace(Set(script1)) |>
          transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(
                Some(
                  KeepRaw(
                    Redeemers(
                      Redeemer(
                        tag = Cert,
                        index = 0,
                        data = Data.List(List.empty),
                        exUnits = ExUnits.zero
                      )
                    )
                  )
                )
              )
          |>
          transactionL
              .andThen(txBodyL)
              .refocus(_.certificates)
              .replace(
                TaggedSet(
                  Certificate.UnregCert(Credential.ScriptHash(script1.scriptHash), coin = None)
                )
              )
          |>
          redeemersL.replace(
            List(
              DetachedRedeemer(
                datum = Data.List(List.empty),
                purpose = ForCert(
                  UnregCert(
                    Credential.ScriptHash(
                      ScriptHash.fromHex("36137e3d612d23a644283f10585958085aa255bdae4076fcefe414b6")
                    ),
                    None
                  )
                )
              )
            )
          )
    )

    // witness = PlutusScriptCredential (ScriptValue script1) RedeemerDatum.unit
    val witness =
        PlutusScriptCredential(ScriptWitness.ScriptValue(script1, Set.empty), Data.List(List.empty))

    //      testBuilderStepsFail
    //        "deregistering stake credential with unneeded witness fails"
    //        [ IssueCertificate (StakeDeregistration $ wrap $ pubKeyHashCredential1)
    //            $ Just witness
    //        ] $
    //        UnneededDeregisterWitness (wrap $ pubKeyHashCredential1) witness
    testBuilderStepsFail(
      label = "Deregistering stake credential with unneeded witness fails",
      steps = List(IssueCertificate(UnregCert(pubKeyHashCredential1, coin = None), Some(witness))),
      error = UnneededDeregisterWitness(StakeCredential(pubKeyHashCredential1), witness)
    )

    // testBuilderStepsFail
    //      "deregistering stake credential with wrong witness fails"
    //      [ IssueCertificate
    //          ( StakeDeregistration $ wrap $ ScriptHashCredential $
    //              PlutusScript.hash script2
    //          )
    //          $ Just
    //          $ PlutusScriptCredential (ScriptValue script1) RedeemerDatum.unit
    //      ] $
    //      IncorrectScriptHash (Right script1) (PlutusScript.hash script2)

    testBuilderStepsFail(
      label = "deregistering stake credential with wrong witness fails",
      steps = List(
        IssueCertificate(
          cert = UnregCert(Credential.ScriptHash(script2.scriptHash), coin = None),
          witness =
              Some(PlutusScriptCredential(ScriptValue(script1, Set.empty), Data.List(List.empty)))
        )
      ),
      error = IncorrectScriptHash(Right(script1), script2.scriptHash)
    )

    // =======================================================================
    // Group: "Modify Aux Data"
    // =======================================================================
    testBuilderSteps(
      label = "ModifyAuxData: id",
      steps = List(ModifyAuxData(identity)),
      expected = Context.empty(Mainnet).toTuple
    )

}

// ===========================================================================
// Test Helpers
// ===========================================================================

// ===========================================================================
// Test Data
// ===========================================================================

val unitRedeemer: Redeemer =
    Redeemer(
      tag = Spend,
      index = 0,
      data = ByteString.fromHex("").toData,
      exUnits = ExUnits.zero
    )

/*
script1 :: PlutusScript
script1 =
    unsafePartial $ fromJust $ flip PlutusScript.decodeCbor PlutusV1 $ wrap $
    hexToByteArrayUnsafe
    "4d01000033222220051200120011"
 */
val script1: Script.PlutusV1 = {
    val bytes = ByteString.fromHex("4d01000033222220051200120011").bytes
    Cbor.decode(bytes).to[Script.PlutusV1].value
}

/*
scriptHash1 :: ScriptHash
scriptHash1 = PlutusScript.hash script1
 */
val scriptHash1: ScriptHash = script1.scriptHash

/*
scriptHashCredential1 :: Credential
scriptHashCredential1 = ScriptHashCredential scriptHash1
 */
val scriptHashCredential1: Credential = Credential.ScriptHash(scriptHash1)

/*
skhOutput :: TransactionOutput
skhOutput =
    TransactionOutput
        { address: EnterpriseAddress
            { networkId: MainnetId
                , paymentCredential: wrap scriptHashCredential1
            }
            , amount: Value (Coin (BigNum.fromInt 5000000)) MultiAsset.empty
            , datum: Nothing
            , scriptRef: Nothing
        }
 */
val skhOutput: TransactionOutput.Babbage = Babbage(
  address = ShelleyAddress(
    network = Mainnet,
    payment = ShelleyPaymentPart.Script(scriptHashCredential1.scriptHashOption.get),
    delegation = null
  ),
  value = Value(Coin(5_000_000L)),
  datumOption = None,
  scriptRef = None
)

/*
pubKeyHashCredential1 :: Credential
pubKeyHashCredential1 =
    PubKeyHashCredential $ unsafePartial
    $ fromJust
        $ decodeCbor
        $ wrap
        ( byteArrayFromIntArrayUnsafe

            )
 */
val pubKeyHashCredential1: Credential = {
    val bytes: Array[Byte] = Array(57, 3, 16, 58, 231, 6, 129, 67, 155, 84, 118, 254, 245, 159, 67,
      155, 139, 200, 109, 132, 191, 178, 211, 118, 252, 63, 86, 23).map(_.toByte)
    Credential.KeyHash(Hash(ByteString.fromArray(bytes)))
}

val pkhOutputPaymentPart: ShelleyPaymentPart = {
    val bytes: Array[Byte] = Array(243, 63, 250, 132, 253, 242, 10, 0, 52, 67, 165, 226, 118, 142,
      18, 233, 45, 179, 21, 53, 220, 166, 32, 136, 177, 83, 223, 36).map(_.toByte)
    ShelleyPaymentPart.Key(Hash(ByteString.fromArray(bytes)))
}

/*
pkhOutput :: TransactionOutput
pkhOutput =
    ( TransactionOutput
        { address: BaseAddress
            { networkId: MainnetId
                , paymentCredential: wrap $ PubKeyHashCredential $ unsafePartial
                $ fromJust
                $ decodeCbor
                $ wrap
                $
                byteArrayFromIntArrayUnsafe
                [ 243
                , 63
                , 250
                , 132
                , 253
                , 242
                , 10
                , 0
                , 52
                , 67
                , 165
                , 226
                , 118
                , 142
                , 18
                , 233
                , 45
                , 179
                , 21
                , 53
                , 220
                , 166
                , 32
                , 136
                , 177
                , 83
                , 223
                , 36
                ]
                , stakeCredential: wrap pubKeyHashCredential1
            }
            , amount: Value (Coin (BigNum.fromInt 5000000)) MultiAsset.empty
            , datum: Nothing
            , scriptRef: Nothing
        }
        )
 */
val pkhOutput: Babbage = Babbage(
  address = ShelleyAddress(
    network = Mainnet,
    payment = pkhOutputPaymentPart,
    delegation = Key(pubKeyHashCredential1.keyHashOption.get.asInstanceOf[StakeKeyHash])
  ),
  value = Value(Coin(5_000_000L)),
  datumOption = None,
  scriptRef = None
)

/*
mkTransactionInput :: String -> Int -> TransactionInput
mkTransactionInput txId ix =
    TransactionInput
        { transactionId: unsafePartial $ fromJust $ decodeCbor $ wrap $
            hexToByteArrayUnsafe txId
            , index: UInt.fromInt ix
        }
 */
def mkTransactionInput(txId: String, ix: Int): TransactionInput = {
    val txIdBytes: Array[Byte] = ByteString.fromHex(txId).bytes
    TransactionInput(
      transactionId = Hash(ByteString.fromArray(txIdBytes)),
      index = ix
    )
}

/*
input0 :: TransactionInput
input0 = mkTransactionInput
"5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
0
 */

val input0: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 0)

/*
input1 :: TransactionInput
input1 = mkTransactionInput
"5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
1
 */
val input1: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 1)

/*
input2 :: TransactionInput
input2 = mkTransactionInput
"5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
2
 */
val input2: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 2)

/*
script2 :: PlutusScript
script2 =
    unsafePartial $ fromJust $ flip PlutusScript.decodeCbor PlutusV1 $ wrap $
    hexToByteArrayUnsafe
    "4e4d01000033222220051200120012"
 */
val script2: Script.PlutusV1 =
    Cbor.decode(ByteString.fromHex("4e4d01000033222220051200120012").bytes)
        .to[Script.PlutusV1]
        .value

/*
anyNetworkTx :: Transaction
anyNetworkTx = Transaction.empty
 */
val anyNetworkTx: Transaction = emptyTransaction

/*
testnetTransaction :: Transaction
testnetTransaction = Transaction.empty # _body <<< _networkId .~ Just TestnetId
 */
// See: https://github.com/mlabs-haskell/purescript-cardano-types/blob/348fbbefa8bec5050e8492f5a9201ac5bb17c9d9/test/CSLHex.purs#L109
val testnetTransaction: Transaction =
    txBodyL.refocus(_.networkId).replace(Some(0))(anyNetworkTx)

val testnetContext: ContextTuple =
    Context.empty(Testnet).toTuple |> transactionL.replace(testnetTransaction)

private def fromRight[A, B](e: Either[A, B]): B =
    e match { case Right(x) => x }

// The fields of a Context, to cut down on noise
private type ContextTuple = (
    Transaction,
    Seq[DetachedRedeemer],
    Network,
    Set[ExpectedSigner],
    Set[TransactionUnspentOutput]
)

def transactionL: Lens[ContextTuple, Transaction] = Focus[ContextTuple](_._1)
def redeemersL: Lens[ContextTuple, Seq[DetachedRedeemer]] = Focus[ContextTuple](_._2)
def networkL: Lens[ContextTuple, Network] = Focus[ContextTuple](_._3)
def expectedSignersL: Lens[ContextTuple, Set[ExpectedSigner]] = Focus[ContextTuple](_._4)
def resolvedUtxosL: Lens[ContextTuple, Set[TransactionUnspentOutput]] = Focus[ContextTuple](_._5)

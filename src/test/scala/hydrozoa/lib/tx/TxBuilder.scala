package hydrozoa.lib.tx

import hydrozoa.emptyTransaction
import hydrozoa.lib.tx.*
import hydrozoa.lib.tx.CredentialWitness.PlutusScriptCredential
import hydrozoa.lib.tx.ExpectedWitnessType.ScriptHashWitness
import hydrozoa.lib.tx.InputAction.SpendInput
import hydrozoa.lib.tx.OutputWitness.{NativeScriptOutput, PlutusScriptOutput}
import hydrozoa.lib.tx.ScriptWitness.ScriptValue
import hydrozoa.lib.tx.TransactionBuilder.modifyTransaction
import hydrozoa.lib.tx.TransactionBuilderStep.*
import hydrozoa.lib.tx.TransactionEditor.{editTransaction, editTransactionSafe}
import hydrozoa.lib.tx.TxBuildError.{
    IncorrectScriptHash,
    UnneededDeregisterWitness,
    WrongNetworkId,
    WrongOutputType
}
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Key
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Certificate.UnregCert
import scalus.cardano.ledger.RedeemerTag.{Cert, Spend}
import scalus.cardano.ledger.Timelock.AllOf
import scalus.cardano.ledger.TransactionOutput.Babbage

import scala.collection.immutable.{SortedMap, SortedSet}

class TxEditorTests extends munit.ScalaCheckSuite {
    // let
    //    oneInput = anyNetworkTx
    //      # _witnessSet <<< _redeemers .~
    //          [ Redeemer
    //              { index: BigNum.zero
    //              , data: RedeemerDatum.unit
    //              , tag: Spend
    //              , exUnits: ExUnits.empty
    //              }
    //          ]
    //      # _body <<< _inputs .~
    //          [ input1 ]
    val oneInput: Transaction = anyNetworkTx
        .focus(_.witnessSet.redeemers)
        .replace(
          Some(
            KeepRaw(
              Redeemers(
                Redeemer(
                  tag = Spend,
                  index = 0,
                  data = ByteString.fromHex("").toData,
                  exUnits = ExUnits.zero
                )
              )
            )
          )
        )
        .focus(_.body.value.inputs)
        .replace(TaggedOrderedSet(input1))

    test("do nothing")({
        assertEquals(obtained = editTransaction(identity)(oneInput), expected = oneInput)
    })

    /*
        test "attach one input to the end" do
      let
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input1, input2 ]
      editTransaction (_body <<< _inputs <>~ [ input2 ]) oneInput
        `shouldEqual` tx'
     */
    test("attach one input to the end")({
        val expectedTx = anyNetworkTx
            .focus(_.witnessSet.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    Redeemer(
                      tag = Spend,
                      index = 0,
                      data = ByteString.fromHex("").toData,
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )
            .focus(_.body.value.inputs)
            .replace(TaggedOrderedSet(input1, input2))

        assertEquals(
          obtained = editTransaction((tx: Transaction) =>
              tx.focus(_.body.value.inputs)
                  .modify((i: TaggedOrderedSet[TransactionInput]) =>
                      TaggedOrderedSet.from(i.toSeq :+ input2)
                  )
          )(
            oneInput
          ),
          expected = expectedTx
        )
    })

    /*
    test "remove two inputs, before and after" do
      let
        tx = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.one
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~ [ input0, input1, input2 ]
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input1 ]
      editTransactionSafe (_body <<< _inputs .~ [ input1 ]) tx
        `shouldEqual` pure tx'
     */
    test("remove two inputs, before and after")({
        val tx1 = anyNetworkTx
            .focus(_.witnessSet.redeemers)
            .replace(Some(KeepRaw(Redeemers(unitRedeemer.focus(_.index).replace(1)))))
            .focus(_.body.value.inputs)
            .replace(TaggedOrderedSet(input0, input1, input2))
        val tx2 = anyNetworkTx
            .focus(_.witnessSet.redeemers)
            .replace(Some(KeepRaw(Redeemers(unitRedeemer))))
            .focus(_.body.value.inputs)
            .replace(TaggedOrderedSet(input1))
        assertEquals(
          obtained = editTransactionSafe((tx: Transaction) =>
              tx.focus(_.body.value.inputs).replace(TaggedOrderedSet(input1))
          )(tx1),
          expected = Right(tx2)
        )
    })

    /*
     test "remove two inputs with redeemers, before and after" do
      let
        tx = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.one
                  , data: wrap $ List []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.fromInt 2
                  , data: wrap $ Map []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~ [ input0, input1, input2 ]
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: wrap $ List []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input1 ]
      editTransactionSafe (_body <<< _inputs .~ [ input1 ]) tx
        `shouldEqual` pure tx'
     */
    test("remove two inputs with redeemers, before and after")({
        val tx1 = anyNetworkTx
            .focus(_.witnessSet.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    unitRedeemer,
                    Redeemer(
                      tag = Spend,
                      index = 1,
                      data = Data.List(List()),
                      exUnits = ExUnits.zero
                    ),
                    Redeemer(
                      tag = Spend,
                      index = 2,
                      data = Data.Map(List.empty),
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )
            .focus(_.body.value.inputs)
            .replace(TaggedOrderedSet(input0, input1, input2))
        val tx2 = anyNetworkTx
            .focus(_.witnessSet.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    Redeemer(
                      tag = Spend,
                      index = 0,
                      data = Data.List(List.empty),
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )
            .focus(_.body.value.inputs)
            .replace(TaggedOrderedSet(input1))
        assertEquals(
          expected = Right(tx2),
          obtained = editTransactionSafe(tx =>
              tx.focus(_.body.value.inputs).replace(TaggedOrderedSet(input1))
          )(tx1)
        )
    })

    /*
    test "remove input & redeemer, add another input & redeemer" do
      let
        tx = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.one
                  , data: wrap $ Map []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~ [ input1, input2 ]
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              -- order is swapped because of `nub`...
              [ Redeemer
                  { index: BigNum.one
                  , data: wrap $ Map []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.zero
                  , data: wrap $ List []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input0, input2 ]
      editTransactionSafe
        ( (_body <<< _inputs .~ [ input0, input2 ]) >>>
            ( _witnessSet <<< _redeemers <>~
                [ Redeemer
                    { index: BigNum.zero
                    , data: wrap $ List []
                    , tag: Spend
                    , exUnits: ExUnits.empty
                    }
                ]
            )
        )
        tx
        `shouldEqual` pure tx'
     */
    test("remove input & redeemer, add another input & redeemer")({
        val tx1 = anyNetworkTx
            .focus(_.witnessSet.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    unitRedeemer,
                    Redeemer(
                      tag = Spend,
                      index = 1,
                      data = Data.Map(List.empty),
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )
            .focus(_.body.value.inputs)
            .replace(TaggedOrderedSet(input1, input2))
        val tx2: Transaction = anyNetworkTx
            .focus(_.witnessSet.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    Redeemer(
                      tag = Spend,
                      index = 1,
                      data = Data.Map(List.empty),
                      exUnits = ExUnits.zero
                    ),
                    Redeemer(
                      tag = Spend,
                      index = 0,
                      data = Data.List(List.empty),
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )
            .focus(_.body.value.inputs)
            .replace(TaggedOrderedSet(input0, input2))
        assertEquals(
          expected = Right(tx2),
          obtained = editTransactionSafe(tx =>
              tx.focus(_.body.value.inputs)
                  .replace(TaggedOrderedSet(input0, input2))
                  .focus(_.witnessSet.redeemers)
                  .replace(
                    Some(
                      KeepRaw(
                        Redeemers(
                          Redeemer(
                            tag = Spend,
                            index = 0,
                            data = Data.List(List.empty),
                            exUnits = ExUnits.zero
                          )
                        )
                      )
                    )
                  )
          )(tx1)
        )
    })
}

class TxBuilderTests extends munit.ScalaCheckSuite {
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
            val res = TransactionBuilder.buildTransaction(steps)
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
        expected: Transaction
    )(implicit loc: munit.Location): Unit =
        test(label)({
            val res = TransactionBuilder.buildTransaction(steps)
            assertEquals(obtained = res, expected = Right(expected))
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
    // ns = ScriptAll []
    val ns: Script.Native = Script.Native(AllOf(IndexedSeq.empty))
    // nsWitness = NativeScriptOutput $ ScriptValue ns
    val nsWitness = NativeScriptOutput(ScriptValue(ns))
    //  plutusScriptWitness =
    //      PlutusScriptOutput (ScriptValue script2) RedeemerDatum.unit
    //        Nothing
    val plutusScriptWitness = PlutusScriptOutput(ScriptValue(script2), Data.List(List()), None)
    //    plutusScriptRefWitness =
    //      PlutusScriptOutput (ScriptReference input1 SpendInput) RedeemerDatum.unit
    //        Nothing
    val plutusScriptRefWitness = PlutusScriptOutput(
      ScriptWitness.ScriptReference(input1, SpendInput),
      Data.List(List()),
      None
    )

    ///////////////////////////////////////////////////////////////
    // Group: "SpendOutput"
    ///////////////////////////////////////////////////////////////

    // testBuilderSteps "PKH output" [ SpendOutput pkhUtxo Nothing ] $
    //      anyNetworkTx # _body <<< _inputs .~ [ input1 ]
    testBuilderSteps(
      label = "PKH Output",
      steps = List(SpendOutput(pkhUtxo, None)),
      expected = anyNetworkTx.focus(_.body.value.inputs).replace(TaggedOrderedSet(input1))
    )

    // testBuilderSteps "PKH output x2 -> 1"
    //      [ SpendOutput pkhUtxo Nothing, SpendOutput pkhUtxo Nothing ] $
    //      anyNetworkTx # _body <<< _inputs .~ [ input1 ]
    testBuilderSteps(
      label = "PKH output x2 -> 1",
      steps = List(SpendOutput(pkhUtxo, None), SpendOutput(pkhUtxo, None)),
      expected = anyNetworkTx.focus(_.body.value.inputs).replace(TaggedOrderedSet(input1))
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
      steps = List(SpendOutput(pkhUtxo, Some(plutusScriptRefWitness))),
      error = WrongOutputType(ScriptHashWitness(plutusScriptRefWitness), pkhUtxo)
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
      steps = List(SpendOutput(skhUtxo, Some(plutusScriptWitness))),
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
        val res = modifyTransaction(testnetTransaction, List(SpendOutput(pkhUtxo, None)))
        assertEquals(obtained = res, expected = Left(WrongNetworkId(pkhUtxo.output.address)))
    })

    // =======================================================================
    // Group: "Pay"
    // =======================================================================

    //     testBuilderSteps "#1" [ Pay pkhOutput ] $
    //      anyNetworkTx # _body <<< _outputs .~ [ pkhOutput ]
    testBuilderSteps(
      label = "Pay #1",
      steps = List(Pay(pkhOutput)),
      expected = anyNetworkTx.focus(_.body.value.outputs).replace(IndexedSeq(Sized(pkhOutput)))
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
            ScriptWitness.ScriptValue(script1),
            redeemer = Data.List(List.empty)
          )
        )
      ),
      expected = anyNetworkTx
          .focus(_.body.value.mint)
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
          .focus(_.witnessSet.plutusV1Scripts)
          .replace(Set(script1))
          .focus(_.witnessSet.redeemers)
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
            PlutusScriptCredential(ScriptWitness.ScriptValue(script1), Data.List(List.empty))
          )
        )
      ),
      expected = anyNetworkTx
          .focus(_.witnessSet.plutusV1Scripts)
          .replace(Set(script1))
          .focus(_.witnessSet.redeemers)
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
          .focus(_.body.value.certificates)
          .replace(
            TaggedSet(Certificate.UnregCert(Credential.ScriptHash(script1.scriptHash), coin = None))
          )
    )

    // witness = PlutusScriptCredential (ScriptValue script1) RedeemerDatum.unit
    val witness = PlutusScriptCredential(ScriptWitness.ScriptValue(script1), Data.List(List.empty))

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
          witness = Some(PlutusScriptCredential(ScriptValue(script1), Data.List(List.empty)))
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
      expected = anyNetworkTx
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
    payment = {
        val bytes: Array[Byte] = Array(243, 63, 250, 132, 253, 242, 10, 0, 52, 67, 165, 226, 118,
          142, 18, 233, 45, 179, 21, 53, 220, 166, 32, 136, 177, 83, 223, 36).map(_.toByte)
        ShelleyPaymentPart.Key(Hash(ByteString.fromArray(bytes)))
    },
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
    emptyTransaction.focus(_.body.value.networkId).replace(Some(0))

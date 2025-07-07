package hydrozoa.l2.block

/*
Contains unit tests for block creation. These tests

 */

import hydrozoa.*
import hydrozoa.infra.plutusAddressAsL2
import hydrozoa.l1.multisig.state.{DepositDatum, DepositTag, DepositUtxos}
import hydrozoa.l2.block.BlockTypeL2.{Major, Minor}
import hydrozoa.l2.ledger.simple.SimpleL2Ledger
import hydrozoa.l2.ledger.*
import org.scalacheck.Arbitrary
import scalus.builtin.Builtins.serialiseData
import scalus.builtin.ByteString
import scalus.builtin.ToData.toData
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.ArbitraryInstances.given
import scalus.prelude.Option

// Helper values
val emptyDeposits : DepositUtxos = TaggedUtxoSet[L1, DepositTag](UtxoSet[L1]())

val dummyHeader : BlockHeader = BlockHeader(0,Minor,0,0,0,"")

////
// Used to construct "singletonDeposit"
val txId: TxId = TxId("a6ce90a9a5ef8ef73858effdae375ba50f302d3c6c8b587a15eaa8fa98ddf741")
val txIx = TxIx(0)
val utxoId = UtxoIdL1(txId, txIx)

val dummyAddress = Arbitrary.arbitrary[Address].sample.get

val dummyBs: ByteString = Arbitrary.arbitrary[ByteString].sample.get
val mbDummyBs = scalus.prelude.Option.Some(dummyBs)
val dd: DepositDatum = DepositDatum(
    address = dummyAddress, datum = mbDummyBs, deadline = 0, refundAddress = dummyAddress, refundDatum = mbDummyBs
)
val dummyAddressBech = AddressBech("addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te")

val output: Output[L1] =
    Output(
        address = dummyAddressBech
        , coins = 1000000
        , tokens = Map.empty
        , mbInlineDatum = Some(serialiseData(dd.toData).toHex)
    )

val singletonDeposit : DepositUtxos = {

    TaggedUtxoSet(UtxoSet[L1](Map((utxoId, output))))
}

val singletonActive :  Map[UtxoIdL2, OutputL2] =
    Map((UtxoIdL2(txId, txIx), Output(dummyAddressBech, 1000000, Map.empty,
        scalus.prelude.Option.asScala(mbDummyBs).map(_.toHex))))

class BlockProducerSpec extends munit.ScalaCheckSuite {

    test("BlockProducer sanity check, negative case") {
        val b = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            emptyDeposits,
            dummyHeader,
            0,
            false
        )
        assert(b.isEmpty, "Sanity check should not produce a block")
    }

    //////////
    // Deposit Tests
    test("BlockProducer sanity check, positive case (no events, one deposit)") {
        val b = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            0,
            false
        )
        assert(b.isDefined, "Sanity check should produce a block")
    }

    // TODO: we can expand this into a property test by generating multiple deposits and ensure that they all
    // appear in the block.
    test("BlockProducer absorbs single deposit in `depositsAbsorbed`; testing 4.2.4.b.i") {
        val (b, _, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            0,
            false
        ).get

        assert(b.blockBody.depositsAbsorbed == Seq(utxoId), "Singleton deposit should appear in the block body")
    }

    // FIXME: case match on results and provide better failure messages
    // FIXME: This test is not currently using the commitment hash, because the method is private
    // TODO: Turn into property test by adding many deposits
    test("BlockProducers inserts single deposit in utxosAdded; testing 4.2.4.b.ii") {
        val (actualBlock, actualUtxosAdded, _, _, _) =
            BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            0,
            false
        ).get

        val expectedUtxoAdded: Output[L2] =
            Output(
                address = plutusAddressAsL2(dummyAddress)
                , coins = output.coins
                , tokens = output.tokens
                , mbInlineDatum = dd.datum.asScala.map(_.toHex)
            )


        // FIXME: this is wrong -- it should be using the commitment hash, but that method is currently private.
        val expectedL2UtxoTxId = txId
        val expectedL2UtxoId: UtxoId[L2] = UtxoIdL2(expectedL2UtxoTxId, TxIx(actualBlock.blockBody.depositsAbsorbed.indexOf(utxoId)))

        val expected: HydrozoaL2Ledger.LedgerUtxoSetOpaque =
            SimpleL2Ledger.mkLedgerForBlockProducer(
                Map((expectedL2UtxoId, expectedUtxoAdded))).getUtxosActive

        //// Unlifting for direct comparison
        val expectedUnlifted = SimpleL2Ledger.unliftUtxoSet(expected)
        val actualUnlifted = SimpleL2Ledger.unliftUtxoSet(actualUtxosAdded)

        val expectedUtxo = expectedUnlifted.iterator.next()
        val actualUtxo = expectedUnlifted.iterator.next()

        // N.B.: we want to be able to check total equality like this: if utxosAdded == expected
        assert(
            expectedUnlifted.size == actualUnlifted.size
                && expectedUnlifted.size == 1
                // Note: comparing on everything EXCEPT TxId
                && expectedUtxo._1.outputIx == actualUtxo._1.outputIx
                && expectedUtxo._2 == actualUtxo._2
        , "Singleton deposit should appear correctly in L2 utxosActive")
    }

    test("BlockProducers should not absorb deposit if `finalizing` is True; testing 4.2.4") {
        val (actualBlock, _, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            0,
            true // finalizing is true
        ).get

        assert(actualBlock.blockBody.depositsAbsorbed == Seq.empty,
          "Singleton deposit should not appear in the block body when finalizing")
    }

    test("BlockProducers should not add deposit to UTxo active if `finalizing` is True; testing 4.2.4") {
        val (_, actualUtxosAdded, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            0,
            true // finalizing is true
        ).get

        assert(SimpleL2Ledger.unliftUtxoSet(actualUtxosAdded).isEmpty,
            "Singleton deposit should not appear in utxosAdded when finalizing")
    }

    test("BlockProducers should not add expired deposit to depositsAbsorbed; testing 4.2.4.a") {
        val (actualBlock, _, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            10000, // this is the important part; deadline is 0, time created is 10k
            true
        ).get

        assert(actualBlock.blockBody.depositsAbsorbed == Seq.empty,
            "Expired singleton deposit should not appear in depositsAbsorbed")
    }

    test("BlockProducers should not add deposit to UTxo active if `finalizing` is True; testing 4.2.4.a") {
        val (_, actualUtxosAdded, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            10000, // this is the important part; deadline is 0, time created is 10k
            true
        ).get

        assert(SimpleL2Ledger.unliftUtxoSet(actualUtxosAdded).isEmpty,
            "Expired singleton deposit should not appear in utxosAdded")
    }

    test("blockNum correctly incremented on block production; 4.2.7.a"){
            val (actualBlock, _,_,_,_) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
                Seq.empty,
                singletonDeposit,
                dummyHeader,
                0,
                false
            ).get

            val actualNum = actualBlock.blockHeader.blockNum
            val expectedNum = dummyHeader.blockNum + 1

            assert(actualNum == expectedNum,
              "Block Number not incremented correctly; expected " ++ expectedNum.toString ++ ", but got " ++ actualNum.toString)
    }

    test("Block type for singleton deposit should be major; 4.2.6.b") {
        val (actualBlock, _, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            0,
            false
        ).get
        assert(actualBlock.blockHeader.blockType == Major,  "Block type incorrect; expected Major, but got " ++ actualBlock.blockHeader.blockType.toString)
    }

    test("Block major version should increment with a singleton deposit; 4.2.7.c.i") {
        val (actualBlock, _, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader,
            0,
            false
        ).get
        assert(actualBlock.blockHeader.versionMajor == dummyHeader.versionMajor + 1, "Block Number not incremented correctly")
    }

    test("Block minor version should be set to zero with a singleton deposit; 4.2.7.c.ii") {
        val (actualBlock, _, _, _, _) = BlockProducer.createBlock(SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq.empty,
            singletonDeposit,
            dummyHeader.copy(versionMinor = 1),
            0,
            false
        ).get
        assert(actualBlock.blockHeader.versionMinor == 0, "Block minor version not reset correctly")
    }

    /////////////////////////////
    // Withdrawal tests
    val singletonWithdrawalEvent : L2EventWithdrawal =
        mkWithdrawalEvent(L2Withdrawal(List(singletonActive.iterator.next()._1)))

    test("BlockProducer sanity check, negative case (one withdrawal, no deposits, missing activeUtxo)") {
        val b = BlockProducer.createBlock(
            SimpleL2Ledger.mkLedgerForBlockProducer(Map.empty),
            Seq(singletonWithdrawalEvent),
            emptyDeposits,
            dummyHeader,
            0,
            false
        )
        assert(b.isDefined, "Singleton withdrawal with empty activeUtxos should produce a block")
    }

    test("BlockProducer sanity check, positive case (one withdrawal, no deposits)") {
        val b = BlockProducer.createBlock(
            SimpleL2Ledger.mkLedgerForBlockProducer(singletonActive),
            Seq(singletonWithdrawalEvent),
            emptyDeposits,
            dummyHeader,
            0,
            false
        )
        assert(b.isDefined, "Singleton withdrawal should produce a block")
    }

    test("Singleton withdrawal removes from singleton active Utxos") {
        val (_, actualActiveUtxos, _,_,_) = BlockProducer.createBlock(
            SimpleL2Ledger.mkLedgerForBlockProducer(singletonActive),
            Seq(singletonWithdrawalEvent),
            emptyDeposits,
            dummyHeader,
            0,
            false
        ).get
        assert(SimpleL2Ledger.unliftUtxoSet(actualActiveUtxos).isEmpty,
            "Singleton withdrawal should clear singleton activeUtxos")
    }

    test("Singleton withdrawal inserts into utxosWithdrawn") {
        val (_ , _, _, actualUtxosWithdrawn, _) = BlockProducer.createBlock(
            SimpleL2Ledger.mkLedgerForBlockProducer(singletonActive),
            Seq(singletonWithdrawalEvent),
            emptyDeposits,
            dummyHeader,
            0,
            false
        ).get
        assert(actualUtxosWithdrawn.utxoMap.iterator.next()._1 == singletonWithdrawalEvent.withdrawal.inputs.head,
            "Singleton withdrawal doesn't affect utxosWithdrawn properly")
    }

    test("Singleton withdrawal creates valid event") {
        val (actualBlock, _, _, _, _) = BlockProducer.createBlock(
            SimpleL2Ledger.mkLedgerForBlockProducer(singletonActive),
            Seq(singletonWithdrawalEvent),
            emptyDeposits,
            dummyHeader,
            0,
            false
        ).get
        
        val expectedEventsValid = Seq((singletonWithdrawalEvent.eventId,L2EventLabel.L2EventWithdrawalLabel))
        
        assert(actualBlock.blockBody.eventsValid == expectedEventsValid,
            "singleton withdrawal from singleton activeUtxos should create a single valid event")
    }


}
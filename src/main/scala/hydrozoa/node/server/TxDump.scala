package hydrozoa.node.server

import hydrozoa.TxL2
import hydrozoa.infra.serializeTxHex
import hydrozoa.l1.multisig.tx.{InitTx, MultisigTx, MultisigTxTag}

object TxDump:
    private val txDumpL1: os.Path = os.pwd / "txsl1.out"
    private val txDumpL2: os.Path = os.pwd / "txsl2.out"

    def dumpInitTx(initTx: InitTx): Unit =
//        TxDump.resetLogs()
//        dumpMultisigTx(initTx)
        ()

    def dumpMultisigTx[T <: MultisigTxTag](tx: MultisigTx[T]): Unit =
//        val cbor = serializeTxHex(tx)
//        if os.isFile(txDumpL1) then os.write.append(txDumpL1, "\n" + cbor)
//        else os.write(txDumpL1, cbor)
        ()

    def dumpL2Tx(tx: TxL2): Unit =
//        val cbor = serializeTxHex(tx)
//        if os.isFile(txDumpL2) then os.write.append(txDumpL2, "\n" + cbor)
//        else os.write(txDumpL2, cbor)
        ()

    private def resetLogs(): Unit =
        os.remove(txDumpL1)
        os.remove(txDumpL2)

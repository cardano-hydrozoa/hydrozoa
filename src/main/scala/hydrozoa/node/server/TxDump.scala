package hydrozoa.node.server

import hydrozoa.infra.serializeTxHex
import hydrozoa.l1.multisig.tx.MultisigTxs.InitializationTx
import hydrozoa.{AnyLevel, TxAny, L1, L2, Tx}

object TxDump:
    private val txDump: os.Path = os.pwd / "txs.out"
    private val txDumpL1: os.Path = os.pwd / "txsL1.out"
    private val txDumpL2: os.Path = os.pwd / "txsL2.out"

    def dumpTx[L <: L1](tx: Tx[L]): Unit =
        val txCbor = serializeTxHex(tx)
        tx match
            case l1Tx: Tx[L1] =>
                l1Tx match
                    case initTx: InitializationTx =>
                        TxDump.resetLogs
                        os.write(txDump, txCbor)
                        os.write(txDumpL1, txCbor)
                    case anyL1Tx =>
                        os.write.append(txDump, "\n" + txCbor)
                        os.write.append(txDumpL1, "\n" + txCbor)
            case l2Tx: Tx[L2] =>
                if os.isFile(txDumpL2) then os.write.append(txDumpL2, "\n" + txCbor)
                else os.write(txDumpL2, txCbor)

    private def resetLogs: Unit =
        os.remove(txDump)
        os.remove(txDumpL1)
        os.remove(txDumpL2)

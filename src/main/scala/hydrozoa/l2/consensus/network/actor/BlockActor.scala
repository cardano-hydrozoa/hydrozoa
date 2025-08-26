package hydrozoa.l2.consensus.network.actor

import hydrozoa.l2.ledger.{L2EventTransaction, L2EventWithdrawal}

import scala.collection.mutable

class BlockActor {
    private val mempool : mutable.Buffer[L2EventTransaction | L2EventWithdrawal] = mutable.Buffer.empty
    def getMempool: Seq[L2EventTransaction | L2EventWithdrawal] = mempool.toList
    def appendToMempool(l2Event: L2EventWithdrawal | L2EventTransaction): Unit = mempool.append(l2Event)
}

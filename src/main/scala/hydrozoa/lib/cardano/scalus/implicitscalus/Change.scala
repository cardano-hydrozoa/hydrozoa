package hydrozoa.lib.cardano.scalus.implicitscalus

import hydrozoa.config.head.network.CardanoNetwork
import scalus.cardano.txbuilder as scalusTx
import scalus.cardano.txbuilder.DiffHandler

object Change {

    def changeOutputDiffHandler(index: Int)(using config: CardanoNetwork.Section): DiffHandler =
        scalusTx.Change.changeOutputDiffHandler(_, _, config.cardanoProtocolParams, index)

}

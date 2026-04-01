// TODO: I'm not sure if this is useful or not. I'm leaning towards "no".

//package hydrozoa.rulebased.ledger.l1.utxo
//
//import hydrozoa.config.head.multisig.fallback.FallbackContingency
//import hydrozoa.config.head.network.CardanoNetwork
//import hydrozoa.config.head.peers.HeadPeers
//import hydrozoa.lib.cardano.scalus.ledger.{CollateralOutput, CollateralUtxo}
//import hydrozoa.multisig.consensus.peer.HeadPeerNumber
//import scalus.cardano.address.ShelleyDelegationPart
//import scalus.cardano.address.ShelleyDelegationPart.Null
//import scalus.cardano.ledger.{AddrKeyHash, Coin, TransactionInput}
//import scalus.uplc.builtin.Builtins.blake2b_224
//
//private type ImplicitConfig = CardanoNetwork.Section & FallbackContingency.Section &
//    HeadPeers.Section
//
///** A wrapper around a [[CollateralUtxo]] encoding the assumptions
//  */
//case class DisputeCollateralUtxo(
//    input: TransactionInput,
//    disputeCollateralOutput: DisputeCollateralOutput
//) {
//
//    def toCollateralUtxo(using config: ImplicitConfig): CollateralUtxo =
//        CollateralUtxo(
//          input = this.input,
//          collateralOutput = this.disputeCollateralOutput.toCollateralOutput
//        )
//}
//
//case class DisputeCollateralOutput(
//    peerNumber: HeadPeerNumber,
//    // Includes fixed collateral, equity payout, and fees for dispute resolution
//    coin: Coin
//) {
//
//    def toCollateralOutput(using
//        config: ImplicitConfig
//    ): CollateralOutput =
//        CollateralOutput(
//
//          addrKeyHash = AddrKeyHash(blake2b_224(config.headPeerVKey(this.peerNumber).get)),
//          delegationPart = Null,
//          coin = coin,
//          datumOption = None,
//          scriptRef = None
//        )
//}

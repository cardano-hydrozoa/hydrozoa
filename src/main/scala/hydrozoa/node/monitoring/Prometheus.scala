package hydrozoa.node.monitoring

import hydrozoa.l2.block.BlockTypeL2
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel.{
    TransactionL2EventLabel,
    WithdrawalL2EventLabel
}
import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.model.snapshots.Unit as PUnit

def nonGenesisEventLabel(eventType: NonGenesisL2EventLabel | String): String = {
    val eventTypeLabel = eventType match
        case TransactionL2EventLabel => "transaction"
        case WithdrawalL2EventLabel  => "withdrawal"
        case str: String             => str
    eventTypeLabel
}

class PrometheusMetrics:

    // JvmMetrics.builder.register // initialize the out-of-the-box JVM metrics

    // Head uptime, seconds
    private val headUptime = Gauge.builder
        .name("headUptime")
        .help("seconds passed since head's initialization")
        .register

    def updateHeadUptime(uptime: Long): Unit = headUptime.set(uptime)

    // Number of blocks
    private val blockNum = Gauge.builder
        .name("blockNum")
        .help("number of minor and major blocks produced by the head and total")
        .labelNames("blockType") // minor / major / total
        .register

    def resetBlocksCounter(): Unit =
        blockNum.clear()

    def incBlocksMinor(): Unit =
        blockNum.labelValues("total").inc()
        blockNum.labelValues("minor").inc()

    def incBlocksMajor(): Unit =
        blockNum.labelValues("total").inc()
        blockNum.labelValues("major").inc()

    // Detailed size of blocks
    private val blockSize = Histogram.builder
        .name("blockSize")
        .help(
          "number of events in blocks (deposit / transaction / withdrawal)"
        )
        .labelNames(
          // "blockType", // Type of block
          "eventType" // Type of event (deposit / transaction / withdrawal )
          // "eventValidity" // valid/invalid
        )
        .register

    def clearBlockSize(): Unit = blockSize.clear()

    def observeBlockSize(
        // blockType: BlockTypeL2,
        eventType: NonGenesisL2EventLabel | String,
        // validity: String,
        number: Int
    ): Unit =
//        val blockTypeLabel = blockType match
//            case Minor => "minor"
//            case Major => "major"
//            case Final => "final"
        blockSize
//            .labelValues(blockTypeLabel, nonGenesisEventLabel(eventType), validity)
            .labelValues(nonGenesisEventLabel(eventType))
            .observe(number)

    // L2 Events in pool
    private val poolEventsL2 = Gauge.builder
        .name("poolEventsL2")
        .help("number of L2 events in pool by type")
        .labelNames(
          "eventType" // Type of event (transaction / withdrawal)
        )
        .register

    def setPoolEventsL2(eventType: NonGenesisL2EventLabel, number: Int): Unit =
        poolEventsL2.labelValues(nonGenesisEventLabel(eventType)).set(number)

    // L2 Events handled
    private val eventsL2Handled = Counter.builder
        .name("eventsL2Handled")
        .help("number of L2 events handled by head by types")
        .labelNames(
          "eventType", // Type of event (transaction / withdrawal)
          "eventValidity" // Vaildity of event (valid / invalid)
        )
        .register

    def incEventsL2Handled(eventType: NonGenesisL2EventLabel, valid: Boolean, number: Int): Unit =
        eventsL2Handled
            .labelValues(nonGenesisEventLabel(eventType), if valid then "valid" else "invalid")
            .inc(number)

    // Deposit queue size
    private val depositQueueSize = Gauge.builder
        .name("depositQueueSize")
        .help("the current size of deposit queue")
        .register

    def setDepositQueueSize(size: Int): Unit = depositQueueSize.set(size.toDouble)

    // Deposit absorbed/returned
    private val deposits = Counter.builder
        .name("deposits")
        .help("number of deposits absorbed / returned")
        .labelNames("state") // absorbed / returned
        .register

    def incAbsorbedDeposits(howMany: Int): Unit = deposits.labelValues("absorbed").inc(howMany)

    // Liquidity
    private val liquidity = Gauge.builder
        .name("liquidity")
        .help("head's liquidity by type (treasury / deposits / rollouts)")
        .labelNames("utxoType") // treasury / deposits / rollouts
        .register

    def clearLiquidity(): Unit =
        liquidity.clear()

    def setTreasuryLiquidity(coins: Long): Unit =
        liquidity.labelValues("treasury").set(coins.toDouble)

    def setDepositsLiquidity(coins: Long): Unit =
        liquidity.labelValues("deposits").set(coins.toDouble)

    private val volume = Counter.builder
        .name("volume")
        .help(
          "historical volume by type: inboundL1 = sum of all L1 deposits, " +
              "outboundL1 = sum of all L1 withdrawal payouts, " +
              "transactedL2 = sum of all L2 tx outputs, " +
              "feesL1 = sum of all L1 fees paid"
        )
        .labelNames("volumeType") // inboundL1 / outboundL1 / transactedL2 / feesL1
        .register

    // FIXME: wire in
    def addInboundL1Volume(increase: Long): Unit = volume.labelValues("inboundL1").inc(increase)
    // FIXME: wire in
    def addOutboundL1Volume(increase: Long): Unit = volume.labelValues("outboundL1").inc(increase)
    def addTransactedL2Volume(increase: Long): Unit =
        volume.labelValues("transactedL2").inc(increase)
    // FIXME: wire in
    def addFeesL1Volume(increase: Long): Unit = volume.labelValues("feesL1").inc(increase)

    // Costs
    private val costs = Histogram.builder
        .name("costs")
        .labelNames("costType") // deposit / settlement
        .register

    // FIXME: wire in
    def observeDepositCost(amount: Long): Unit =
        costs.labelValues("deposit").observe(amount.toDouble)

    def observeSettlementCost(amount: Long): Unit =
        costs.labelValues("settlement").observe(amount.toDouble)

// Other ideas to add:
//  - number of actors
//  - consensus interaction duration
//  - L2 state: liquidity/number of utxo/addresses

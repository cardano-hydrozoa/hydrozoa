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

def nonGenesisEventLabel(eventType: NonGenesisL2EventLabel) = {
    val eventTypeLabel = eventType match
        case TransactionL2EventLabel => "transaction"
        case WithdrawalL2EventLabel  => "withdrawal"
    eventTypeLabel
}

class PrometheusMetrics:

    // JvmMetrics.builder.register // initialize the out-of-the-box JVM metrics

    // Head uptime, seconds
    private val headUptime = Gauge.builder
        .name("headUptime")
        .register

    def updateHeadUptime(uptime: Long): Unit = headUptime.set(uptime)

    // Number of blocks
    //  - total
    //  - minor
    //  - major
    private val blocks = Counter.builder
        .name("blocks_total")
        .help("total number of blocks produced by a head")
        .labelNames("blockType")
        .register

    def incBlocksMinor(): Unit =
        blocks.labelValues("total").inc()
        blocks.labelValues("minor").inc()

    def incBlocksMajor(): Unit =
        blocks.labelValues("total").inc()
        blocks.labelValues("major").inc()

    val eventsInBlockUnit = PUnit("eventsInBlock")

    // Size of blocks
    private val blockEvents = Histogram.builder
        .name("number_of_block_events")
        .help("The total ")
        .unit(eventsInBlockUnit)
        .labelNames(
          "blockType", // Type of block
          "eventType", // Type of event (transaction / withdrawal)
          "eventValidity" // valid/invalid
        )
        .register

    def observeBlockSize(
        blockType: BlockTypeL2,
        eventType: NonGenesisL2EventLabel,
        number: Int
    ): Unit =
        val blockTypeLabel = blockType match
            case Minor => "minor"
            case Major => "major"
            case Final => "final"
        blockEvents.labelValues(blockTypeLabel, nonGenesisEventLabel(eventType)).observe(number)

    // L2 Events in pool
    private val poolEventsL2 = Gauge.builder
        .name("poolEventsL2")
        .labelNames(
          "eventType" // Type of event (transaction / withdrawal)
        )
        .register

    def setPoolEventsL2(eventType: NonGenesisL2EventLabel, number: Int): Unit =
        poolEventsL2.labelValues(nonGenesisEventLabel(eventType)).set(number)

    // L2 Events in blocks
    private val eventsL2Handled = Counter.builder
        .name("eventsL2Handled")
        .labelNames(
          "eventType" // Type of event (transaction / withdrawal)
        )
        .register

    def incEventsL2Handled(eventType: NonGenesisL2EventLabel, number: Int): Unit =
        eventsL2Handled.labelValues(nonGenesisEventLabel(eventType)).inc(number)

    // Deposit queue size
    private val depositQueueSize = Gauge.builder
        .name("depositQueueSize")
        .register

    def setDepositQueueSize(size: Int): Unit = depositQueueSize.set(size.toDouble)

    // Deposit absorbed/returned
    private val deposits = Counter.builder
        .name("deposits")
        .labelNames("state") // absorbed / returned
        .register

    def incAbsorbedDeposits(howMany: Int): Unit = deposits.labelValues("absorbed").inc(howMany)

    // Liquidity
    private val liquidity = Gauge.builder
        .name("liquidity")
        .labelNames("utxoType") // treasury / deposits / rollouts
        .register

    def setTreasuryLiquidity(coins: Long): Unit =
        liquidity.labelValues("treasury").set(coins.toDouble)

    def setDepositsLiquidity(coins: Long): Unit =
        liquidity.labelValues("deposits").set(coins.toDouble)

    private val volume = Counter.builder
        .name("volume")
        .labelNames("volumeType") // inboundL1 / outboundL1 / transactedL2 / feesL1
        .register

    def addInboundL1Volume(increase: Long): Unit = volume.labelValues("inboundL1").inc(increase)
    def addOutboundL1Volume(increase: Long): Unit = volume.labelValues("outboundL1").inc(increase)
    def addTransactedL2Volume(increase: Long): Unit =
        volume.labelValues("transactedL2").inc(increase)
    def addFeesL1Volume(increase: Long): Unit = volume.labelValues("feesL1").inc(increase)

    // Costs
    private val costs = Histogram.builder
        .name("costs")
        .labelNames("costType") // deposit / settlement
        .register

    def observeDepositCost(amount: Long): Unit = costs.labelValues("deposit").observe(amount.toDouble)

    def observeSettlementCost(amount: Long): Unit = costs.labelValues("settlement").observe(amount.toDouble)

// Other ideas to add:
    //  - number of actors
    //  - consensus interaction duration

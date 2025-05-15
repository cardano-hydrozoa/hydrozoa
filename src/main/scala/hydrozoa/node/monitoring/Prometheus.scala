package hydrozoa.node.monitoring

import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel.{
    TransactionL2EventLabel,
    WithdrawalL2EventLabel
}
import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}

def nonGenesisEventLabel(eventType: NonGenesisL2EventLabel | String): String = {
    val eventTypeLabel = eventType match
        case TransactionL2EventLabel => "transaction"
        case WithdrawalL2EventLabel  => "withdrawal"
        case str: String             => str
    eventTypeLabel
}

trait Metrics:
    def updateHeadUptime(uptime: Long): Unit
    def resetBlocksCounter(): Unit
    def incBlocksMinor(): Unit
    def incBlocksMajor(): Unit
    def clearBlockSize(): Unit
    def observeBlockSize(eventType: NonGenesisL2EventLabel | String, number: Int): Unit
    def setPoolEventsL2(eventType: NonGenesisL2EventLabel, number: Int): Unit
    def incEventsL2Handled(eventType: NonGenesisL2EventLabel, valid: Boolean, number: Int): Unit
    def setDepositQueueSize(size: Int): Unit
    def incAbsorbedDeposits(howMany: Int): Unit
    def clearLiquidity(): Unit
    def setTreasuryLiquidity(coins: Long): Unit
    def setDepositsLiquidity(coins: Long): Unit
    def addInboundL1Volume(increase: Long): Unit
    def addOutboundL1Volume(increase: Long): Unit
    def addTransactedL2Volume(increase: Long): Unit
    def addFeesL1Volume(increase: Long): Unit
    def observeDepositCost(amount: Long): Unit
    def observeSettlementCost(amount: Long): Unit

class NoopMetrics extends Metrics:
    override def updateHeadUptime(uptime: Long): Unit = ()
    override def resetBlocksCounter(): Unit = ()
    override def incBlocksMinor(): Unit = ()
    override def incBlocksMajor(): Unit = ()
    override def clearBlockSize(): Unit = ()
    override def observeBlockSize(
        eventType: NonGenesisL2EventLabel | String,
        number: Int
    ): Unit = ()
    override def setPoolEventsL2(eventType: NonGenesisL2EventLabel, number: Int): Unit = ()
    override def incEventsL2Handled(
        eventType: NonGenesisL2EventLabel,
        valid: Boolean,
        number: Int
    ): Unit = ()
    override def setDepositQueueSize(size: Int): Unit = ()
    override def incAbsorbedDeposits(howMany: Int): Unit = ()
    override def clearLiquidity(): Unit = ()
    override def setTreasuryLiquidity(coins: Long): Unit = ()
    override def setDepositsLiquidity(coins: Long): Unit = ()
    override def addInboundL1Volume(increase: Long): Unit = ()
    override def addOutboundL1Volume(increase: Long): Unit = ()
    override def addTransactedL2Volume(increase: Long): Unit = ()
    override def addFeesL1Volume(increase: Long): Unit = ()
    override def observeDepositCost(amount: Long): Unit = ()
    override def observeSettlementCost(amount: Long): Unit = ()

object NoopMetrics:
    def apply(): Metrics = new NoopMetrics()

class PrometheusMetrics extends Metrics:

    // JvmMetrics.builder.register // initialize the out-of-the-box JVM metrics

    // Head uptime, seconds
    private val headUptime = Gauge.builder
        .name("headUptime")
        .help("seconds passed since head's initialization")
        .register

    override def updateHeadUptime(uptime: Long): Unit = headUptime.set(uptime.toDouble)

    // Number of blocks
    private val blockNum = Gauge.builder
        .name("blockNum")
        .help("number of minor and major blocks produced by the head and total")
        .labelNames("blockType") // minor / major / total
        .register

    override def resetBlocksCounter(): Unit =
        blockNum.clear()

    override def incBlocksMinor(): Unit =
        blockNum.labelValues("total").inc()
        blockNum.labelValues("minor").inc()

    override def incBlocksMajor(): Unit =
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

    override def clearBlockSize(): Unit = blockSize.clear()

    override def observeBlockSize(
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

    override def setPoolEventsL2(eventType: NonGenesisL2EventLabel, number: Int): Unit =
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

    override def incEventsL2Handled(
        eventType: NonGenesisL2EventLabel,
        valid: Boolean,
        number: Int
    ): Unit =
        eventsL2Handled
            .labelValues(nonGenesisEventLabel(eventType), if valid then "valid" else "invalid")
            .inc(number)

    // Deposit queue size
    private val depositQueueSize = Gauge.builder
        .name("depositQueueSize")
        .help("the current size of deposit queue")
        .register

    override def setDepositQueueSize(size: Int): Unit = depositQueueSize.set(size.toDouble)

    // Deposit absorbed/returned
    private val deposits = Counter.builder
        .name("deposits")
        .help("number of deposits absorbed / returned")
        .labelNames("state") // absorbed / returned
        .register

    override def incAbsorbedDeposits(howMany: Int): Unit =
        deposits.labelValues("absorbed").inc(howMany)

    // Liquidity
    private val liquidity = Gauge.builder
        .name("liquidity")
        .help("head's liquidity by type (treasury / deposits / rollouts)")
        .labelNames("utxoType") // treasury / deposits / rollouts
        .register

    override def clearLiquidity(): Unit =
        liquidity.clear()

    override def setTreasuryLiquidity(coins: Long): Unit =
        liquidity.labelValues("treasury").set(coins.toDouble)

    override def setDepositsLiquidity(coins: Long): Unit =
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

    override def addInboundL1Volume(increase: Long): Unit =
        volume.labelValues("inboundL1").inc(increase)
    override def addOutboundL1Volume(increase: Long): Unit =
        volume.labelValues("outboundL1").inc(increase)
    override def addTransactedL2Volume(increase: Long): Unit =
        volume.labelValues("transactedL2").inc(increase)
    override def addFeesL1Volume(increase: Long): Unit = volume.labelValues("feesL1").inc(increase)

    // Costs
    private val costs = Histogram.builder
        .name("costs")
        .labelNames("costType") // deposit / settlement
        .register

    // FIXME: wire in
    override def observeDepositCost(amount: Long): Unit =
        costs.labelValues("deposit").observe(amount.toDouble)

    override def observeSettlementCost(amount: Long): Unit =
        costs.labelValues("settlement").observe(amount.toDouble)

// Other ideas to add:
//  - number of actors
//  - consensus interaction duration
//  - L2 state: liquidity/number of utxo/addresses

object PrometheusMetrics:
    def apply(): Metrics = new PrometheusMetrics()

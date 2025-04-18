package hydrozoa.node.monitoring

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.instrumentation.jvm.JvmMetrics

class PrometheusMetrics:

    // JvmMetrics.builder.register // initialize the out-of-the-box JVM metrics

    private val blocks = Counter.builder
        .name("blocks_total")
        .help("total number of blocks produced by a head")
        .labelNames("type")
        .register
    
    def incBlocksMinor(): Unit =
        blocks.labelValues("total").inc()
        blocks.labelValues("minor").inc()

    def incBlocksMajor(): Unit =
        blocks.labelValues("total").inc()
        blocks.labelValues("major").inc()

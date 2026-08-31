package hydrozoa.config.head

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.given
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import java.io.ByteArrayOutputStream
import java.lang.Double.doubleToLongBits
import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, Hash32, ScriptHash, TransactionInput}
import scalus.uplc.builtin.{ByteString, platform}

/** The digest that pins a head's agreed configuration, as defined in `design/head-params-hash.md`.
  *
  * It covers the **whole head config**, not only the [[parameters.HeadParameters]] case class: the
  * head parameters, the L1 network, the per-peer equity split, the script references, block zero's
  * timing, and the coil hub topology. Peers never exchange their configs, so this is what makes a
  * disagreement visible — the multisig treasury datum carries it, and a peer that computes a
  * different value cannot parse the initialization transaction and so never signs block zero.
  *
  * ```
  * headParamsHash = blake2b_256(
  *      "gummiworm-head-params-v1"
  *   || <HeadParameters, in declaration order>
  *   || <cardanoNetwork> || <initialEquityContributions> || <scriptReferences>
  *   || <initialBlockTiming> || <coilHubTopology>
  * )
  * ```
  *
  * The layout is written out byte by byte rather than delegating to a JSON or CBOR encoder.
  * `QuantizedFiniteDuration`, `Coin` and `PositiveInt` each have their own codec quirks, and a
  * codec tweak that silently moved this value — once it is written into a treasury datum — would
  * leave a live head unable to parse its own initialization transaction.
  *
  * See `design/head-params-hash.md` for what each field is doing here, what is deliberately left
  * out, and the checks that compare this value.
  */
object HeadParamsHash {

    /** Mixed in before anything else so this digest can never collide with a hash of the same bytes
      * taken for another purpose. ASCII, no terminator — the fixed-width field that follows makes
      * the boundary unambiguous.
      */
    val domainTag: Array[Byte] = "gummiworm-head-params-v1".getBytes(UTF_8)

    def apply(config: HeadConfig.Section): Hash32 = {
        val out = Buffer()
        out.raw(domainTag)

        // -- HeadParameters.txTiming
        out.duration(config.minSettlementDuration.convert)
        out.duration(config.inactivityMarginDuration.convert)
        out.duration(config.silenceDuration.convert)
        out.duration(config.depositSubmissionDuration.convert)
        out.duration(config.depositMaturityDuration.convert)
        out.duration(config.depositAbsorptionDuration.convert)

        // -- HeadParameters.fallbackContingency
        val collective = config.collectiveContingency
        out.coin(collective.publicVoteDeposit)
        out.coin(collective.fallbackTxFee)
        out.coin(collective.minAdaForTreasury)
        out.coin(collective.minAdaForRegime)
        val individual = config.individualContingency
        out.coin(individual.collateralDeposit)
        out.coin(individual.tallyTxFee)
        out.coin(individual.voteDeposit)
        out.coin(individual.voteTxFee)

        // -- HeadParameters: disputeResolutionConfig / settlementConfig / blockConfig
        out.duration(config.votingDuration)
        out.u32(config.maxDepositsAbsorbedPerBlock.convert)
        out.u32(config.maxRequestsPerBlock.convert)
        out.u32(config.backpressureCoefficient.convert)

        // -- HeadParameters.rateLimits. Every knob of the block gate, not only the spacing period:
        // the backlog limits and the smoothing shape decide when a leader cuts a block under load,
        // so two peers agreeing on the period alone would still cut different blocks.
        out.finiteDuration(config.softBlockMinPeriod)
        out.finiteDuration(config.hardStackMinPeriod)
        out.u32(config.blockBacklogSoftLimit)
        out.u32(config.blockBacklogHardLimit)
        out.f64(config.blockGateFloor)
        out.f64(config.blockGateSmoothing)
        out.finiteDuration(config.blockGateSlice)

        // -- HeadParameters: the rest
        out.u32(config.coilQuorum)
        out.hash32(config.l2ParamsHash)
        out.framed(config.l2Ledger.configString.getBytes(UTF_8))
        out.bool(config.identityIsomorphism)

        // -- cardanoNetwork. `networkId` is scalus's own id byte, which covers `Network.Other` too;
        // `protocolMagic` alone does not determine it, because `CardanoNetwork.Custom` pairs an
        // arbitrary `CardanoInfo` with an arbitrary magic. The protocol params are absent on
        // purpose: they are fetched from the chain and move with hard forks, so they are not
        // something the peers agree on.
        out.u64(config.protocolMagic)
        out.u8(config.network.networkId)
        val slotConfig = config.slotConfig
        out.u64(slotConfig.zeroTime)
        out.u64(slotConfig.zeroSlot)
        out.u64(slotConfig.slotLength)

        // -- initialEquityContributions, ascending by HeadPeerNumber. The per-peer split, not just
        // the total: only the total reaches the treasury value.
        val equity = config.initialEquityContributions.toSortedMap
        out.u32(equity.size)
        equity.foreach { (peer, coin) =>
            out.u32(peer.convert)
            out.coin(coin)
        }

        // -- scriptReferences. Pin an output reference exactly where the chain pins one, and the
        // hash everywhere else: the two Plutus scripts contribute their hashes (a build mismatch
        // nothing else catches), the setup ladder its rung-0 outref (what the regime datum
        // records as `setupG2Ladder`).
        out.scriptHash(HydrozoaBlueprint.treasuryScriptHash)
        out.scriptHash(HydrozoaBlueprint.disputeScriptHash)
        out.transactionInput(config.setupLadderAnchor)

        // -- initialBlockTiming. Only `startTime` and `endTime` reach the initialization
        // transaction's validity end; the other three reach no transaction at all.
        val header = config.initialBlock.blockBrief.header
        out.instant(header.startTime.convert)
        out.instant(header.endTime.convert)
        out.instant(header.fallbackTxStartTime.convert)
        out.instant(header.forcedMajorBlockWakeupTime.convert)
        header.mDepositDecisionWakeupTime match {
            case None => out.bool(false)
            case Some(wakeup) =>
                out.bool(true)
                out.instant(wakeup.convert)
        }

        // -- coilHubTopology, ascending by CoilPeerNumber. Coil peer numbers are contiguous from
        // zero, so a hub's position in the sequence is its coil peer number and is not repeated.
        val coilPeers = config.coilPeers
        val hubs = coilPeers.coilPeerNumbers.flatMap(coilPeers.hubHeadPeerNumber)
        out.u32(hubs.size)
        hubs.foreach(hub => out.u32(hub.convert))

        Hash[Blake2b_256, Any](platform.blake2b_256(ByteString.unsafeFromArray(out.bytes)))
    }

    /** Accumulates the preimage. Variable-width values are length-framed and fixed-width ones are
      * not, so no two distinct configs can produce the same byte string.
      */
    private final class Buffer {
        private val buffer = ByteArrayOutputStream()

        def bytes: Array[Byte] = buffer.toByteArray

        def raw(value: Array[Byte]): Unit = buffer.write(value)

        def framed(value: Array[Byte]): Unit = {
            u32(value.length)
            buffer.write(value)
        }

        def u8(value: Int): Unit = buffer.write(value & 0xff)

        def u32(value: Int): Unit = {
            buffer.write((value >>> 24) & 0xff)
            buffer.write((value >>> 16) & 0xff)
            buffer.write((value >>> 8) & 0xff)
            buffer.write(value & 0xff)
        }

        def u64(value: Long): Unit = {
            u32((value >>> 32).toInt)
            u32(value.toInt)
        }

        def bool(value: Boolean): Unit = u8(if value then 0x01 else 0x00)

        /** IEEE-754 bits, big-endian. `doubleToLongBits` rather than the raw variant, so every NaN
          * collapses to one canonical bit pattern and the digest cannot depend on which NaN a
          * decoder happened to produce.
          */
        def f64(value: Double): Unit = u64(doubleToLongBits(value))

        def coin(value: Coin): Unit = u64(value.value)

        def duration(value: QuantizedFiniteDuration): Unit = finiteDuration(value.finiteDuration)

        def finiteDuration(value: FiniteDuration): Unit = u64(value.toMillis)

        def instant(value: QuantizedInstant): Unit = u64(value.instant.toEpochMilli)

        def hash32(value: Hash32): Unit = raw(value.bytes)

        def scriptHash(value: ScriptHash): Unit = raw(value.bytes)

        def transactionInput(value: TransactionInput): Unit = {
            raw(value.transactionId.bytes)
            u32(value.index)
        }
    }
}

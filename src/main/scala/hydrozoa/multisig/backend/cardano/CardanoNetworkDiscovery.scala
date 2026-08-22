package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import scala.concurrent.ExecutionContext
import scalus.cardano.address.Network
import scalus.cardano.blockfrost.GenesisInfo
import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, SlotConfig}
import scalus.cardano.node.BlockfrostProvider

/** Discovers a [[CardanoNetwork.Custom]] — its [[CardanoInfo]] plus `protocolMagic` — from a
  * running Blockfrost-compatible backend, so a head can target a chain that is not one of the three
  * standard networks: a private or consortium devnet, a self-hosted `blockfrost-backend-ryo`, the
  * Blockfrost Platform.
  *
  * Everything comes from the Blockfrost API itself: protocol parameters from the provider, and the
  * network id, slot config and magic from the standard `/genesis` endpoint. Nothing here is
  * specific to any one backend implementation.
  *
  * This is a **generation-time** tool, reached through `hydrozoa discover-network`, never from the
  * config-read path: `build-head-config` and `serve` decode a complete chain description out of
  * their config rather than resolving one. Pinning the discovered value also makes a build
  * reproducible — a second `build-head-config` cannot silently pick up a different chain state.
  */
object CardanoNetworkDiscovery {

    private given ExecutionContext = ExecutionContext.global

    /** When a chain starts and how its slots run — everything about a chain's identity that is not
      * its protocol parameters. A Blockfrost `/genesis` reports exactly this, and [[fromGenesis]]
      * converts one; a backend that serves no `/genesis` has to be told instead.
      *
      * @param systemStartSeconds
      *   when slot 0 began, in epoch seconds — as both `/genesis` and Cardano tooling report it.
      * @param slotLengthSeconds
      *   seconds per slot, fractional allowed: a fast devnet may run sub-second slots.
      * @param epochLength
      *   slots per epoch.
      */
    final case class ChainGeometry(
        systemStartSeconds: Long,
        slotLengthSeconds: Double,
        epochLength: Long,
        protocolMagic: Long
    )

    object ChainGeometry {

        /** The geometry a Blockfrost-compatible `/genesis` reported. */
        def fromGenesis(genesis: GenesisInfo): ChainGeometry = ChainGeometry(
          systemStartSeconds = genesis.systemStart,
          slotLengthSeconds = genesis.slotLength.toDouble,
          epochLength = genesis.epochLength,
          protocolMagic = genesis.networkMagic.toLong
        )
    }

    /** Query a running Blockfrost-compatible backend and assemble the chain it serves.
      *
      * @param blockfrostUrl
      *   Blockfrost-compatible API base URL, e.g. `http://localhost:18080/api/v1`.
      * @param apiKey
      *   Blockfrost API key; empty for a keyless endpoint.
      * @param geometry
      *   the chain's slot geometry and magic, for a backend that serves no `/genesis` — a Yaci
      *   devnet answers 404 there, and a minimal Blockfrost implementation may too. Left empty,
      *   `/genesis` is asked, which is the norm.
      */
    def discover(
        blockfrostUrl: String,
        apiKey: String = "",
        geometry: Option[ChainGeometry] = None
    ): IO[CardanoNetwork.Custom] =
        for {
            // Normalize a trailing slash so the sub-clients don't build `…/api/v1//epochs`.
            url <- IO.pure(blockfrostUrl.stripSuffix("/"))
            // `create` needs *some* slot config to fetch params; the real one comes from the
            // geometry below, so the placeholder never reaches the result.
            provider <- IO.fromFuture(
              IO(BlockfrostProvider.create(apiKey, url, Network.Testnet, SlotConfig.preview))
            )
            resolved <- geometry.fold(
              IO.fromFuture(IO(provider.fetchGenesis)).map(ChainGeometry.fromGenesis)
            )(IO.pure)
            custom <- IO.fromEither(
              mkCustomNetwork(resolved, provider.cardanoInfo.protocolParams).left
                  .map(IllegalStateException(_))
            )
        } yield custom

    /** Assemble the chain description from its geometry and parameters. Separated from the fetch so
      * the interesting decisions — the address tag, the seconds-to-milliseconds scaling, and the
      * refusal to impersonate a standard chain — are testable without a running backend, and so
      * that a told geometry and a discovered one are assembled and validated identically.
      *
      * Assumes a **Shelley-at-genesis** chain (`zeroSlot = 0`, `zeroTime = systemStart`). That
      * holds for a freshly created devnet and fails for a Byron-prefixed one, where the Shelley era
      * starts later — which is exactly what [[CardanoNetwork.rejectStandardMagic]] refuses here,
      * since those are the standard chains and they have baked-in slot geometry already.
      */
    private[cardano] def mkCustomNetwork(
        geometry: ChainGeometry,
        protocolParams: ProtocolParams
    ): Either[String, CardanoNetwork.Custom] = {
        val magic = geometry.protocolMagic
        val network =
            if magic == CardanoNetwork.Mainnet.protocolMagic then Network.Mainnet
            else Network.Testnet
        val slotConfig = SlotConfig(
          zeroTime = geometry.systemStartSeconds * 1000L,
          zeroSlot = 0L,
          // Rounded, not truncated: a 0.5s slot has to become 500ms, and 1.0 must not land on 999.
          slotLength = Math.round(geometry.slotLengthSeconds * 1000.0),
          epochLength = geometry.epochLength
        )
        val custom: CardanoNetwork.Custom =
            CardanoNetwork.Custom(CardanoInfo(protocolParams, network, slotConfig), magic)
        for {
            _ <- rejectDegenerateGeometry(slotConfig)
            _ <- CardanoNetwork.rejectStandardMagic(custom)
        } yield custom
    }

    /** Refuse a slot length or epoch length of zero. Both are divisors — `timeToSlot` and `epochOf`
      * — so a zero would not fail here but as an `ArithmeticException` at every node's startup,
      * long after this description was pinned into the head config.
      */
    private def rejectDegenerateGeometry(slotConfig: SlotConfig): Either[String, Unit] = {
        val degenerate = List(
          "slotLength" -> slotConfig.slotLength,
          "epochLength" -> slotConfig.epochLength
        ).filter(_._2 <= 0L)
        Either.cond(
          degenerate.isEmpty,
          (),
          degenerate
              .map((name, value) => s"the backend reports $name = $value")
              .mkString("", "; ", " — a chain cannot be described with that slot geometry")
        )
    }
}

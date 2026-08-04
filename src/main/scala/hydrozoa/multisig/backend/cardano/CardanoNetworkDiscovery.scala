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

    /** Query a running Blockfrost-compatible backend and assemble the chain it serves.
      *
      * @param blockfrostUrl
      *   Blockfrost-compatible API base URL, e.g. `http://localhost:18080/api/v1`.
      * @param apiKey
      *   Blockfrost API key; empty for a keyless endpoint.
      */
    def discover(blockfrostUrl: String, apiKey: String = ""): IO[CardanoNetwork.Custom] =
        for {
            // Normalize a trailing slash so the sub-clients don't build `…/api/v1//epochs`.
            url <- IO.pure(blockfrostUrl.stripSuffix("/"))
            // `create` needs *some* slot config to fetch params; `/genesis` then supplies the real
            // one, so the placeholder never reaches the result.
            provider <- IO.fromFuture(
              IO(BlockfrostProvider.create(apiKey, url, Network.Testnet, SlotConfig.preview))
            )
            genesis <- IO.fromFuture(IO(provider.fetchGenesis))
            custom <- IO.fromEither(
              mkCustomNetwork(genesis, provider.cardanoInfo.protocolParams).left
                  .map(IllegalStateException(_))
            )
        } yield custom

    /** Assemble the chain description from what the backend reported. Separated from the fetch so
      * the interesting decisions — the address tag, the seconds-to-milliseconds scaling, and the
      * refusal to impersonate a standard chain — are testable without a running backend.
      *
      * Assumes a **Shelley-at-genesis** chain (`zeroSlot = 0`, `zeroTime = systemStart`). That
      * holds for a freshly created devnet and fails for a Byron-prefixed one, where the Shelley era
      * starts later — which is exactly what [[CardanoNetwork.rejectStandardMagic]] refuses here,
      * since those are the standard chains and they have baked-in slot geometry already.
      */
    private[cardano] def mkCustomNetwork(
        genesis: GenesisInfo,
        protocolParams: ProtocolParams
    ): Either[String, CardanoNetwork.Custom] = {
        val magic = genesis.networkMagic.toLong
        val network =
            if magic == CardanoNetwork.Mainnet.protocolMagic then Network.Mainnet
            else Network.Testnet
        val slotConfig = SlotConfig(
          zeroTime = genesis.systemStart * 1000L,
          zeroSlot = 0L,
          slotLength = genesis.slotLength * 1000L,
          epochLength = genesis.epochLength
        )
        val custom: CardanoNetwork.Custom =
            CardanoNetwork.Custom(CardanoInfo(protocolParams, network, slotConfig), magic)
        CardanoNetwork.rejectStandardMagic(custom).map(_ => custom)
    }
}

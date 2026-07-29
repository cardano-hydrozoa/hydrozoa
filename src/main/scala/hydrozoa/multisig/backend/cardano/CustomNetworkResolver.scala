package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import scala.concurrent.ExecutionContext
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, SlotConfig}
import scalus.cardano.node.BlockfrostProvider

/** Resolves a [[CardanoNetwork.Custom]] (its [[CardanoInfo]] + `protocolMagic`) live from a
  * Blockfrost-compatible backend, so a head can target an arbitrary network — a Yaci devnet, a
  * self-hosted `blockfrost-backend-ryo`, the Blockfrost Platform — rather than only the three
  * standard networks.
  *
  * Protocol params always come from the Blockfrost `blockfrostUrl` (via scalus's
  * [[BlockfrostProvider]]). The network id, slot config, and protocol magic come from either the
  * standard `/genesis` endpoint (generic) or, for a Yaci devnet whose slot `startTime` moves on
  * each reset, the Yaci admin API.
  */
object CustomNetworkResolver {

    private given ExecutionContext = ExecutionContext.global

    /** Build a [[CardanoNetwork.Custom]] by querying a running Blockfrost-compatible backend.
      *
      * @param blockfrostUrl
      *   Blockfrost-compatible API base URL (protocol params, and — absent `yaciAdminUrl` — the
      *   genesis network id / magic / slot config).
      * @param yaciAdminUrl
      *   Yaci admin API base URL (e.g. `http://host:10000/local-cluster/api`); when given, the slot
      *   config's dynamic `startTime` and the `protocolMagic` come from
      *   `GET {yaciAdminUrl}/admin/devnet` rather than `/genesis` (Yaci's `/genesis` `startTime`
      *   does not track devnet resets).
      * @param apiKey
      *   Blockfrost API key, empty for a keyless devnet endpoint.
      */
    def resolve(
        blockfrostUrl: String,
        yaciAdminUrl: Option[String] = None,
        apiKey: String = ""
    ): IO[CardanoNetwork.Custom] =
        yaciAdminUrl match {
            case Some(adminUrl) => resolveViaYaciAdmin(blockfrostUrl, adminUrl, apiKey)
            case None           => resolveViaGenesis(blockfrostUrl, apiKey)
        }

    private def resolveViaYaciAdmin(
        blockfrostUrl: String,
        adminUrl: String,
        apiKey: String
    ): IO[CardanoNetwork.Custom] =
        for {
            devnet <- fetchYaciDevnetInfo(adminUrl)
            slotConfig = SlotConfig(devnet.startTime * 1000L, 0L, devnet.slotLength * 1000L)
            provider <- IO.fromFuture(
              IO(BlockfrostProvider.create(apiKey, blockfrostUrl, Network.Testnet, slotConfig))
            )
        } yield CardanoNetwork.Custom(provider.cardanoInfo, devnet.protocolMagic.toLong)

    private def resolveViaGenesis(
        blockfrostUrl: String,
        apiKey: String
    ): IO[CardanoNetwork.Custom] =
        for {
            // `create` needs a slot config to fetch params; `/genesis` then supplies the real one.
            bootstrap <- IO.fromFuture(
              IO(
                BlockfrostProvider.create(
                  apiKey,
                  blockfrostUrl,
                  Network.Testnet,
                  SlotConfig.preview
                )
              )
            )
            genesis <- IO.fromFuture(IO(bootstrap.fetchGenesis))
            network =
                if genesis.networkMagic.toLong == CardanoNetwork.Mainnet.protocolMagic then
                    Network.Mainnet
                else Network.Testnet
            slotConfig = SlotConfig(genesis.systemStart * 1000L, 0L, genesis.slotLength * 1000L)
            cardanoInfo = CardanoInfo(bootstrap.cardanoInfo.protocolParams, network, slotConfig)
        } yield CardanoNetwork.Custom(cardanoInfo, genesis.networkMagic.toLong)

    private final case class YaciDevnetInfo(startTime: Long, slotLength: Long, protocolMagic: Int)

    /** `GET {adminUrl}/admin/devnet` → the running Yaci devnet's slot timing + protocol magic. */
    private def fetchYaciDevnetInfo(adminUrl: String): IO[YaciDevnetInfo] =
        IO.blocking {
            val uri = java.net.URI.create(s"${adminUrl.stripSuffix("/")}/admin/devnet")
            val client = java.net.http.HttpClient.newHttpClient()
            val request = java.net.http.HttpRequest.newBuilder(uri).GET().build()
            val response =
                client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
            if response.statusCode() != 200 then
                throw RuntimeException(
                  s"Yaci admin GET $uri failed: ${response.statusCode()} ${response.body()}"
                )
            val json = ujson.read(response.body())
            YaciDevnetInfo(
              startTime = json("startTime").num.toLong,
              slotLength = json("slotLength").num.toLong,
              protocolMagic = json("protocolMagic").num.toInt
            )
        }
}

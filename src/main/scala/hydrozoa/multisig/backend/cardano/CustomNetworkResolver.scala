package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import java.time.Duration
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

    private val connectTimeout = Duration.ofSeconds(10)
    private val requestTimeout = Duration.ofSeconds(30)

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
    ): IO[CardanoNetwork.Custom] = {
        // Normalize a trailing slash so the sub-clients don't build `…/api/v1//epochs`.
        val url = blockfrostUrl.stripSuffix("/")
        val resolved = yaciAdminUrl match {
            case Some(adminUrl) => resolveViaYaciAdmin(url, adminUrl, apiKey)
            case None           => resolveViaGenesis(url, apiKey)
        }
        resolved.flatMap(rejectStandardMagic)
    }

    /** A backend that reports a standard network's magic must be configured as that standard
      * network (`cardanoNetwork: preview|preprod|mainnet` + `cardanoBackendUrl`), not left unset (a
      * bare Custom): only the baked-in [[CardanoInfo]] carries the correct (Byron-aware) slot
      * geometry and address tag. Devnet magics (e.g. Yaci's 42) are unaffected.
      */
    private def rejectStandardMagic(custom: CardanoNetwork.Custom): IO[CardanoNetwork.Custom] = {
        val standard = List(CardanoNetwork.Mainnet, CardanoNetwork.Preprod, CardanoNetwork.Preview)
            .find(_.protocolMagic == custom.protocolMagic)
        standard match {
            case Some(net) =>
                val name = net.toString.toLowerCase
                IO.raiseError(
                  IllegalStateException(
                    s"the backend reports $name's network magic (${custom.protocolMagic}); set " +
                        s"cardanoNetwork: $name with your endpoint as cardanoBackendUrl instead of " +
                        "leaving the network unset, so the correct baked-in parameters and slot " +
                        "config are used"
                  )
                )
            case None => IO.pure(custom)
        }
    }

    private def resolveViaYaciAdmin(
        blockfrostUrl: String,
        adminUrl: String,
        apiKey: String
    ): IO[CardanoNetwork.Custom] =
        for {
            devnet <- fetchYaciDevnetInfo(adminUrl)
            // slotLength is seconds as a Double (a devnet may use a sub-second slot, e.g. 0.5); scale
            // to milliseconds *before* truncating so a fractional slot length is not rounded to 0.
            slotConfig = SlotConfig(
              devnet.startTime * 1000L,
              0L,
              (devnet.slotLengthSeconds * 1000).toLong
            )
            provider <- IO.fromFuture(
              IO(BlockfrostProvider.create(apiKey, blockfrostUrl, Network.Testnet, slotConfig))
            )
        } yield CardanoNetwork.Custom(provider.cardanoInfo, devnet.protocolMagic)

    private def resolveViaGenesis(
        blockfrostUrl: String,
        apiKey: String
    ): IO[CardanoNetwork.Custom] =
        for {
            // `create` needs a slot config to fetch params; `/genesis` then supplies the real one.
            // This assumes a Shelley-at-genesis network (zeroSlot = 0, zeroTime = systemStart), which
            // holds for a Yaci-style devnet; a Byron-prefixed network (real preprod/mainnet) has a
            // later Shelley start and should use its standard `CardanoNetwork`, not `Custom`.
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
            slotConfig = SlotConfig(
              zeroTime = genesis.systemStart * 1000L,
              zeroSlot = 0L,
              slotLength = genesis.slotLength * 1000L,
              epochLength = genesis.epochLength
            )
            cardanoInfo = CardanoInfo(bootstrap.cardanoInfo.protocolParams, network, slotConfig)
        } yield CardanoNetwork.Custom(cardanoInfo, genesis.networkMagic.toLong)

    private final case class YaciDevnetInfo(
        startTime: Long,
        slotLengthSeconds: Double,
        protocolMagic: Long
    )

    /** `GET {adminUrl}/admin/devnet` → the running Yaci devnet's slot timing + protocol magic. */
    private def fetchYaciDevnetInfo(adminUrl: String): IO[YaciDevnetInfo] =
        IO.blocking {
            val uri = URI.create(s"${adminUrl.stripSuffix("/")}/admin/devnet")
            val client = HttpClient.newBuilder().connectTimeout(connectTimeout).build()
            val request = HttpRequest.newBuilder(uri).timeout(requestTimeout).GET().build()
            val response = client.send(request, BodyHandlers.ofString())
            if response.statusCode() != 200 then
                throw RuntimeException(
                  s"Yaci admin GET $uri failed: ${response.statusCode()} ${response.body()}"
                )
            val json = ujson.read(response.body())
            YaciDevnetInfo(
              startTime = json("startTime").num.toLong,
              slotLengthSeconds = json("slotLength").num,
              protocolMagic = json("protocolMagic").num.toLong
            )
        }
}

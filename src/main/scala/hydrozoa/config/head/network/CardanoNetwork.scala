package hydrozoa.config.head.network

import hydrozoa.config.head.rulebased.scripts.RuleBasedScriptAddresses
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, Coin, EvaluatorMode, PlutusScriptEvaluator, ProtocolParams, ProtocolVersion, SlotConfig, TransactionOutput}
import scalus.cardano.txbuilder.TransactionBuilder

export CardanoNetwork.ensureMinAda

type StandardCardanoNetwork =
    CardanoNetwork.Mainnet.type | CardanoNetwork.Preprod.type | CardanoNetwork.Preview.type

enum CardanoNetwork(_cardanoInfo: CardanoInfo, _protocolMagic: Long)
    extends CardanoNetwork.Section {
    case Mainnet extends CardanoNetwork(CardanoInfo.mainnet, 764824073)
    case Preprod extends CardanoNetwork(CardanoInfo.preprod, 1)
    case Preview extends CardanoNetwork(CardanoInfo.preview, 2)
    case Custom(override val cardanoInfo: CardanoInfo, override val protocolMagic: Long)
        extends CardanoNetwork(cardanoInfo, protocolMagic)

    override transparent inline def cardanoNetwork: CardanoNetwork = this

    override def protocolMagic: Long = _protocolMagic
    override def cardanoInfo: CardanoInfo = _cardanoInfo
    override def network: Network = _cardanoInfo.network
    override def slotConfig: SlotConfig = _cardanoInfo.slotConfig
    override def cardanoProtocolParams: ProtocolParams = _cardanoInfo.protocolParams

    override lazy val ruleBasedScriptAddresses: RuleBasedScriptAddresses = RuleBasedScriptAddresses(
      this
    )
}

object CardanoNetwork {
    trait Section extends RuleBasedScriptAddresses.Section {
        def cardanoNetwork: CardanoNetwork

        def ruleBasedScriptAddresses: RuleBasedScriptAddresses =
            cardanoNetwork.ruleBasedScriptAddresses

        def protocolMagic: Long = cardanoNetwork.protocolMagic

        def cardanoInfo: CardanoInfo = cardanoNetwork.cardanoInfo

        def network: Network = cardanoNetwork.network

        def slotConfig: SlotConfig = cardanoNetwork.slotConfig

        def cardanoProtocolParams: ProtocolParams = cardanoNetwork.cardanoProtocolParams

        final def babbageUtxoMinLovelace(serializedSize: PositiveInt): Coin = Coin(
          (160 + serializedSize.convert) * cardanoProtocolParams.utxoCostPerByte
        )

        final def maxNonPlutusTxFee: Coin = Coin(
          cardanoProtocolParams.txFeeFixed + cardanoProtocolParams.maxTxSize * cardanoProtocolParams.txFeePerByte
        )

        final def plutusScriptEvaluatorForTxBuild: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluatorMode.EvaluateAndComputeCost)

        final def cardanoProtocolVersion: ProtocolVersion = cardanoProtocolParams.protocolVersion
    }

    extension [T <: TransactionOutput](self: T)
        def ensureMinAda(config: CardanoNetwork.Section): T =
            TransactionBuilder.ensureMinAda(self, config.cardanoProtocolParams).asInstanceOf[T]

    extension (self: StandardCardanoNetwork)
        def protocolParams: ProtocolParams = self.cardanoProtocolParams
        def slotConfig: SlotConfig = self.slotConfig
        def cardanoInfo: CardanoInfo = self match
            case CardanoNetwork.Mainnet => CardanoInfo.mainnet
            case CardanoNetwork.Preprod => CardanoInfo.preprod
            case CardanoNetwork.Preview => CardanoInfo.preview

    given cardanoNetworkEncoder: Encoder[CardanoNetwork] with {
        override def apply(cn: CardanoNetwork): Json = cn match {
            case CardanoNetwork.Mainnet => "mainnet".asJson
            case CardanoNetwork.Preview => "preview".asJson
            case CardanoNetwork.Preprod => "preprod".asJson
            case CardanoNetwork.Custom(cardanoInfo, protocolMagic) =>
                Json.obj(
                  "custom" -> cardanoInfo.asJson,
                  "protocolMagic" -> cardanoInfo.protocolParams.asJson
                )
        }
    }

    given cardanoNetworkDecoder: Decoder[CardanoNetwork] = {
        val knownNetworkDecoder = Decoder.decodeString.emap {
            case x if x.toLowerCase == "mainnet" => Right(CardanoNetwork.Mainnet)
            case x if x.toLowerCase == "preview" => Right(CardanoNetwork.Preview)
            case x if x.toLowerCase == "preprod" => Right(CardanoNetwork.Preprod)
            case other =>
                Left(
                  "Error decoding the cardano network. Valid values are \"mainnet\", \"preview\","
                      + "\"preprod\", or a map {\"custom\" : (...insert CardanoInfo here...)}."
                )
        }
        val customNetworkDecoder = Decoder.instance(c =>
            for {
                cardanoInfo <- c.downField("custom").as[CardanoInfo]
                protocolMagic <- c.downField("protocolMagic").as[Long]
            } yield CardanoNetwork.Custom(cardanoInfo, protocolMagic)
        )

        List[Decoder[CardanoNetwork]](knownNetworkDecoder, customNetworkDecoder)
            .reduceLeft(_ or _)
    }
}

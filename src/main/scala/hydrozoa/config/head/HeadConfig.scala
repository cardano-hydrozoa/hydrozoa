package hydrozoa.config.head

import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.block.Block

final case class HeadConfig private (
    override val headConfigPreinit: HeadConfig.Preinit,
    override val initialBlockSection: InitialBlock,
) extends HeadConfig.Section {
    override transparent inline def headConfig: HeadConfig = this
}

object HeadConfig {
    private val logger = Logging.logger("HeadConfig")

    def apply(
        headConfigPreinit: HeadConfig.Preinit,
        initialBlock: Block.MultiSigned.Initial
    ): Option[HeadConfig] =
        import headConfigPreinit.*
        apply(cardanoNetwork, headParameters, headPeers, initialBlock, initializationParameters)

    def apply(
        cardanoNetwork: CardanoNetwork,
        headParams: HeadParameters,
        headPeers: HeadPeers,
        initialBlock: Block.MultiSigned.Initial,
        initializationParams: InitializationParameters,
    ): Option[HeadConfig] = {
        for {
            headConfigPreinit <- HeadConfig.Preinit(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams
            )
            result <- Some(new HeadConfig(headConfigPreinit, InitialBlock(initialBlock)))
        } yield result

    }

    trait Section extends HeadConfig.Preinit.Section, InitialBlock.Section {
        def headConfig: HeadConfig

        override def headConfigPreinit: Preinit = headConfig.headConfigPreinit
        def initialBlockSection: InitialBlock = headConfig.initialBlockSection
    }

    final case class Preinit private[head] (
        override val cardanoNetwork: CardanoNetwork,
        override val headParameters: HeadParameters,
        override val headPeers: HeadPeers,
        override val initializationParameters: InitializationParameters,
    ) extends Preinit.Section {
        override transparent inline def headConfigPreinit: Preinit = this
    }

    object Preinit {
        def apply(
            cardanoNetwork: CardanoNetwork,
            headParams: HeadParameters,
            headPeers: HeadPeers,
            initializationParams: InitializationParameters
        ): Option[HeadConfig.Preinit] = {
            val headConfigPreinit = new HeadConfig.Preinit(
              cardanoNetwork,
              headParams,
              headPeers,
              initializationParams
            )

            val isBalancedInitializationFunding = headConfigPreinit.isBalancedInitializationFunding

            if !isBalancedInitializationFunding then {
                logger.error("Initialization funding is unbalanced.")
            }

            Option.when(isBalancedInitializationFunding)(headConfigPreinit)
        }

        trait Section
            extends CardanoNetwork.Section,
              HeadParameters.Section,
              HeadPeers.Section,
              InitializationParameters.Section {
            def headConfigPreinit: HeadConfig.Preinit

            def cardanoNetwork: CardanoNetwork = headConfigPreinit.cardanoNetwork
            def headParameters: HeadParameters = headConfigPreinit.headParameters
            def headPeers: HeadPeers = headConfigPreinit.headPeers
            def initializationParameters: InitializationParameters =
                headConfigPreinit.initializationParameters
        }
    }
}

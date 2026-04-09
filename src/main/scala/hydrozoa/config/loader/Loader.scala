package hydrozoa.config.loader

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.*
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.number.PositiveInt.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerCodecs
import io.bullet.borer.Cbor
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scala.io.{BufferedSource, Source}
import scala.util.Try
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString
import scalus.|>

object Loader {
    def file(name: String = "config.toml"): Resource[IO, BufferedSource] =
        Resource.fromAutoCloseable(IO(Source.fromFile(name)))

    def main(args: Array[String]): Unit =
        file()
            .use(bufferedSource =>
                val raw: String = bufferedSource.getLines().mkString("\n")
                val eParsed = Json.fromString(raw).as[VerificationKey]
                for {
                    parsed <- IO.fromEither(eParsed)
                    _ <- IO.println(parsed)
                } yield ()
            )
            .unsafeRunSync()
}

// Notes to self:
// - CIP0116 json codecs
// - Sever/JsonCodecs
// - RemoteL2LedgerCodecs
object Codecs {
    export FromScalus.{*, given}
    export Derived.{*, given}

    /** These codecs are borrowed from Scalus. Scalus uses a different library for serialization
      * (upickle), which means that error reporting might be strange.
      */
    object FromScalus {
        given protocolParamsDecoder
            : Decoder[ProtocolParams] = Decoder.decodeString.emap(rawString =>
            Try(ProtocolParams.fromBlockfrostJson(rawString)).toEither.left.map(e =>
                "ProtocolParams decoding failed. NOTE: we wrap the scalus blockfrost codec," +
                    s"which uses the upickle JSON library instead of circe. The message from upickle is: $e"
            )
        )

        given protocolParamsEncoder: Encoder[ProtocolParams] with {
            override def apply(pp: ProtocolParams): Json = {
                val scalusSerialization =
                    upickle.write(pp)(using ProtocolParams.blockfrostParamsReadWriter)
                val Right(json) = parser.parse(scalusSerialization): @unchecked
                json
            }
        }
    }

    object Derived {

        import Codecs.*

        given headConfigEncoder: Encoder[HeadConfig] = deriveEncoder[HeadConfig]
        given headConfigDecoder: Decoder[HeadConfig] = Decoder.instance { c =>
            for {
                preinit <- c.downField("headConfigPreinit").as[HeadConfig.Preinit]
                initBlock <- {
                    given HeadConfig.Preinit.Section = preinit
                    c.downField("initialBlock").as[Block.MultiSigned.Initial]
                }

                // TODO: Better error reporting
                hc <- HeadConfig(preinit, initBlock)
                    .toRight(
                      io.circe.DecodingFailure("Initialization funding is unbalanced", c.history)
                    )
            } yield hc
        }
        given headConfigPreinitEncoder: Encoder[HeadConfig.Preinit] with {
            override def apply(a: HeadConfig.Preinit): Json = {
                given CardanoNetwork.Section = a.cardanoNetwork

                deriveEncoder[HeadConfig.Preinit].apply(a)
            }

        }

        given headConfigPreinitDecoder: Decoder[HeadConfig.Preinit] with {
            override def apply(c: HCursor): Decoder.Result[HeadConfig.Preinit] =
                for {
                    cardanoNetwork <- c.downField("cardanoNetwork").as[CardanoNetwork]
                    res <- {
                        given CardanoNetwork.Section = cardanoNetwork

                        deriveDecoder[HeadConfig.Preinit](c)
                    }
                } yield res
        }

        given blockMultisignedInitialEncoder: Encoder[Block.MultiSigned.Initial] =
            deriveEncoder[Block.MultiSigned.Initial]

        given blockMultisignedInitialDecoder(using
            HeadConfig.Preinit.Section
        ): Decoder[Block.MultiSigned.Initial] =
            deriveDecoder[Block.MultiSigned.Initial]

        given cardanoNetworkEncoder: Encoder[CardanoNetwork] = deriveEncoder[CardanoNetwork]

        given cardanoNetworkDecoder: Decoder[CardanoNetwork] = deriveDecoder[CardanoNetwork]

        given blockBriefInitialEncoder: Encoder[BlockBrief.Initial] =
            deriveEncoder[BlockBrief.Initial]

        given blockBriefInitialDecoder(using CardanoNetwork.Section): Decoder[BlockBrief.Initial] =
            deriveDecoder[BlockBrief.Initial]

        given headParametersEncoder: Encoder[HeadParameters] = deriveEncoder[HeadParameters]

        given headParametersDecoder(using CardanoNetwork.Section): Decoder[HeadParameters] =
            deriveDecoder[HeadParameters]

        given blockEffectsMultiSignedInitialEncoder: Encoder[BlockEffects.MultiSigned.Initial] =
            deriveEncoder[BlockEffects.MultiSigned.Initial]

        // TODO: Should these checks be part of the smart constructor instead?
        given blockEffectsMultiSignedInitialDecoder(using
            hcpi: HeadConfig.Preinit.Section
        ): Decoder[BlockEffects.MultiSigned.Initial] =
            Decoder.instance { c =>
                val initParams = hcpi.initializationParams
                val resolvedUtxosSeq =
                    (initParams.initialAdditionalFundingUtxos
                        + initParams.initialSeedUtxo.toTuple).toSeq.map((ti, to) => Utxo(ti, to))
                for {
                    resolvedUtxos <- ResolvedUtxos.empty
                        .addUtxos(resolvedUtxosSeq)
                        .left
                        .map(e =>
                            io.circe.DecodingFailure(
                              "Conflicting resolved utxos are present in the `initialAdditionalFundingUtxos` and/or `initialSeedUtxo`." +
                                  s"Please ensure that the following utxos do not contain duplicate TransactionInputs: $e",
                              c.history
                            )
                        )
                    initTx <- c.downField("initializationTx").as[Transaction]

                    blockEnd: BlockCreationEndTime = {
                        val ttlSlot = initTx.body.value.ttl.get
                        val ttlQi = QuantizedInstant.fromSlot(hcpi.slotConfig, ttlSlot)
                        BlockCreationEndTime(
                          ttlQi - hcpi.txTiming.minSettlementDuration - hcpi.txTiming.inactivityMarginDuration
                        )
                    }
                    fbTx <- c.downField("fallbackTx").as[Transaction]
                    initTxSeq <- InitializationTxSeq
                        .Parse(hcpi)(blockEnd, (initTx, fbTx), resolvedUtxos)
                        .result
                        .left
                        .map(e =>
                            io.circe.DecodingFailure(
                              s"Parsing of the initial block effects failed. Error: $e",
                              c.history
                            )
                        )
                } yield BlockEffects.MultiSigned.Initial(
                  initTxSeq.initializationTx,
                  initTxSeq.fallbackTx
                )

            }

        // TODO: we should take one of "mainnet", "preprod", or "preview", OR "{custom = filepath}"
        given cardanoInfoEncoder: Encoder[CardanoInfo] = deriveEncoder[CardanoInfo]

        given cardanoInfoDecoder: Decoder[CardanoInfo] = deriveDecoder[CardanoInfo]

        given headPeerEncoder: Encoder[HeadPeers] = deriveEncoder[HeadPeers]

        given headPeerDecoder: Decoder[HeadPeers] = deriveDecoder[HeadPeers]

        given initializationParametersEncoder(using
            config: CardanoNetwork.Section
        ): Encoder[InitializationParameters] = deriveEncoder[InitializationParameters]

        given initializationParametersDecoder(using
            config: CardanoNetwork.Section
        ): Decoder[InitializationParameters] = deriveDecoder[InitializationParameters]

        given fallbackContingencyEncoder: Encoder[FallbackContingency] =
            deriveEncoder[FallbackContingency]

        given fallbackContingencyDecoder: Decoder[FallbackContingency] =
            deriveDecoder[FallbackContingency]

        given networkEncoder: Encoder[Network] = deriveEncoder[Network]

        given networkDecoder: Decoder[Network] = deriveDecoder[Network]

        given disputeResolutionConfigEncoder: Encoder[DisputeResolutionConfig] =
            deriveEncoder[DisputeResolutionConfig]

        given disputeResolutionConfigDecoder(using
            CardanoNetwork.Section
        ): Decoder[DisputeResolutionConfig] =
            deriveDecoder[DisputeResolutionConfig]

        given slotConfigEncoder: Encoder[SlotConfig] = deriveEncoder[SlotConfig]

        given slotConfigDecoder: Decoder[SlotConfig] = deriveDecoder[SlotConfig]

        given fallbackContingencyCollectiveEncoder: Encoder[FallbackContingency.Collective] =
            deriveEncoder[FallbackContingency.Collective]

        given fallbackContingencyCollectiveDecoder: Decoder[FallbackContingency.Collective] =
            deriveDecoder[FallbackContingency.Collective]

        given fallbackContingencyIndividualEncoder: Encoder[FallbackContingency.Individual] =
            deriveEncoder[FallbackContingency.Individual]

        given fallbackContingencyIndividualDecoder: Decoder[FallbackContingency.Individual] =
            deriveDecoder[FallbackContingency.Individual]

        given utxoEncoder: Encoder[Utxo] = deriveEncoder[Utxo]

        given utxoDecoder: Decoder[Utxo] = deriveDecoder[Utxo]

        given settlementConfigEncoder: Encoder[SettlementConfig] = deriveEncoder[SettlementConfig]

        given settlementConfigDecoder: Decoder[SettlementConfig] = deriveDecoder[SettlementConfig]

    }

    given quantizedFiniteDurationEncoder: Encoder[QuantizedFiniteDuration] with {
        override def apply(qfd: QuantizedFiniteDuration): Json = qfd.finiteDuration.asJson
    }

    // Silently quantizes according to slot config. Is this what we want?
    given quantizedFiniteDurationDecoder(using
        config: CardanoNetwork.Section
    ): Decoder[QuantizedFiniteDuration] =
        Decoder.instance { c =>
            for {
                fd <- c.as[FiniteDuration]
            } yield QuantizedFiniteDuration(config.slotConfig, fd)
        }

    given headPeerNumberKeyEncoder: KeyEncoder[HeadPeerNumber] =
        KeyEncoder.encodeKeyInt.contramap(_.toInt)

    given headPeerNumberKeyDecoder: KeyDecoder[HeadPeerNumber] with {
        override def apply(s: String): Option[HeadPeerNumber] = {
            for {
                i <- KeyDecoder.decodeKeyInt(s)
                pi <- PositiveInt(i)
            } yield HeadPeerNumber(pi)
        }
    }

    given blockHeaderInitialEncoder: Encoder[BlockHeader.Initial] with {
        def helper(f: BlockHeader.Initial => QuantizedInstant)(using
            bh: BlockHeader.Initial
        ): Json =
            f(bh).instant.asJson

        override def apply(initBH: BlockHeader.Initial): Json = {
            given BlockHeader.Initial = initBH
            Json.obj(
              "startTime" -> helper(_.startTime),
              "endTime" -> helper(_.endTime),
              "fallbackTxStartTime" -> helper(_.fallbackTxStartTime),
              "majorBlockWakeupTime" -> helper(_.majorBlockWakeupTime),
              "kzgCommitment" -> initBH.kzgCommitment.asJson
            )
        }
    }

    given blockHeaderInitialDecoder(using
        config: CardanoNetwork.Section
    ): Decoder[BlockHeader.Initial] =
        Decoder.instance { c =>
            given HCursor = c

            def helper(fieldName: String)(using
                c: HCursor
            ): Either[DecodingFailure, QuantizedInstant] =
                for {
                    instant <- c.downField(fieldName).as[java.time.Instant]
                    res = QuantizedInstant(config.slotConfig, instant)
                } yield res

            for {
                startTime <- helper("startTime")
                endTime <- helper("endTime")
                fbtx <- helper("fallbackTxStartTime")
                mbwt <- helper("majorBlockWakeupTime")
                kzg <- c.downField("kzgCommitment").as[KzgCommitment]
            } yield BlockHeader.Initial(
              BlockCreationStartTime(startTime),
              BlockCreationEndTime(endTime),
              FallbackTxStartTime(fbtx),
              MajorBlockWakeupTime(mbwt),
              kzg
            )
        }
    given txTimingEncoder: Encoder[TxTiming] with {

        def helper(f: TxTiming => QuantizedFiniteDuration)(using txTiming: TxTiming): Json =
            f(txTiming).finiteDuration.asJson(using finiteDurationEncoder)

        override def apply(txTiming: TxTiming): Json = {
            given TxTiming = txTiming

            Json.obj(
              "minSettlementDuration" -> helper(_.minSettlementDuration),
              "inactivityMarginDuration" -> helper(_.inactivityMarginDuration),
              "silenceDuration" -> helper(_.silenceDuration),
              "depositSubmissionDuration" -> helper(_.depositSubmissionDuration),
              "depositMaturityDuration" -> helper(_.depositMaturityDuration),
              "depositAbsorptionDuration" -> helper(_.depositAbsorptionDuration)
            )
        }
    }

    given txTimingDecoder(using config: CardanoNetwork.Section): Decoder[TxTiming] =
        Decoder.instance { c =>
            given HCursor = c

            def helper(fieldName: String)(using c: HCursor) =
                for {
                    fd <- c.downField(fieldName).as[FiniteDuration]
                    res = QuantizedFiniteDuration(config.slotConfig, fd)
                } yield res

            for {
                msd <- helper("minSettlementDuration")
                imd <- helper("inactivityMarginDuration")
                sd <- helper("silenceDuration")
                dsd <- helper("depositSubmissionDuration")
                dmd <- helper("depositMaturityDuration")
                dad <- helper("depositAbsorptionDuration")
            } yield TxTiming(
              MinSettlementDuration(msd),
              InactivityMarginDuration(imd),
              SilenceDuration(sd),
              DepositSubmissionDuration(dsd),
              DepositMaturityDuration(dmd),
              DepositAbsorptionDuration(dad)
            )

        }

    // FIXME: for some reason, it can't find the instance when I doing like this?
    //    given txEncoder[A <: Tx[A]] : Encoder[Tx[A]] =
    //   Encoder.encodeString.contramap(hasTx => hasTx.tx.toCbor |> ByteString.fromArray |> (_.toHex))

    given initializationTxEncoder: Encoder[InitializationTx] =
        Encoder.encodeString.contramap(initTx =>
            initTx.tx.toCbor |> ByteString.fromArray |> (_.toHex)
        )
    given fallbackTxEncoder: Encoder[FallbackTx] =
        Encoder.encodeString.contramap(fallbackTx =>
            fallbackTx.tx.toCbor |> ByteString.fromArray |> (_.toHex)
        )
    given transactionDecoder: Decoder[Transaction] = Decoder.decodeString.emap(hex =>
        val bytes = ByteString.fromHex(hex).bytes
        Try(Transaction.fromCbor(bytes)).toEither.left.map(e =>
            "CBOR decoding of transaction failed. Error Message:" +
                s" $e.getMessage"
        )
    )

    given evacuationMapEncoder(using config: CardanoNetwork.Section): Encoder[EvacuationMap] = {
        val codecs = RemoteL2LedgerCodecs(config)
        Encoder
            .encodeMap[EvacuationKey, Payout.Obligation](using
              evacuationKeyKeyEncoder,
              codecs.payoutObligationEncoder
            )
            .contramap(emap => emap.evacuationMap)
    }

    given evacuationMapDecoder(using config: CardanoNetwork.Section): Decoder[EvacuationMap] = {
        val codecs = RemoteL2LedgerCodecs(config)
        Decoder
            .decodeMap[EvacuationKey, Payout.Obligation](using
              evacuationKeyKeyDecoder,
              codecs.payoutObligationDecoder
            )
            .map(m => EvacuationMap.from(m))
    }

    // Does this need more?
    given evacuationKeyKeyEncoder: KeyEncoder[EvacuationKey] = {
        KeyEncoder.encodeKeyString.contramap(_.byteString.toHex)
    }

    // FIXME: This is partial, but KeyDecoder lacks the "emap" method that Decoder has?
    given evacuationKeyKeyDecoder: KeyDecoder[EvacuationKey] with {
        override def apply(s: String): Option[EvacuationKey] =
            for {
                hex <- KeyDecoder.decodeKeyString(s)
                bytes <- Try(ByteString.fromHex(s)).toOption
                ek <- EvacuationKey(bytes)
            } yield ek
    }

    given headPeerNumberEncoder: Encoder[HeadPeerNumber] = Encoder.encodeInt.contramap(_.toInt)

    given headPeerNumberDecoder: Decoder[HeadPeerNumber] = Decoder.decodeInt.emap(i =>
        Either.cond(
          i >= 0 && i < (1 << 8),
          right = HeadPeerNumber(i),
          left = s"Expected a number `i` such  that `i >= 0 && i < (1 << 8)`, but got $i"
        )
    )

    given finiteDurationEncoder: Encoder[FiniteDuration] with {
        // TODO: Should we encode as a string, like in CIP0116?
        def apply(fd: FiniteDuration): Json = Encoder.encodeLong(fd.toMillis)
    }

    given finiteDurationDecoder(using config: CardanoNetwork.Section): Decoder[FiniteDuration] =
        Decoder.decodeLong.map(l => l.millis)

    // FIXME (maybe?): combine with `given Encoder[KeepRaw[TransactionOutput]]` in RemoteL2LedgerCodecs(?)
    given transactionOutputEncoder: Encoder[TransactionOutput] with {

        def apply(txOut: TransactionOutput): Json = {
            val cbor = Cbor.encode(txOut).toByteArray
            Json.fromString(ByteString.fromArray(cbor).toHex)
        }
    }

    given transactionOutputDecoder: Decoder[TransactionOutput] = Decoder.instance { c =>
        for {
            hex <- c.as[String]
            bytes <- Try(ByteString.fromHex(hex).bytes).toEither.left.map(e =>
                io.circe.DecodingFailure(
                  s"Hex decoding of the transaction output failed. Message: ${e.getMessage}",
                  c.history
                )
            )
            txOut <- Try(Cbor.decode(bytes).to[TransactionOutput].value).toEither.left.map(e =>
                io.circe.DecodingFailure(
                  s"CBOR decoding of the transaction output failed. Message: ${e.getMessage}",
                  c.history
                )
            )
        } yield txOut

    }

    given transactionInputKeyEncoder: KeyEncoder[TransactionInput] =
        KeyEncoder.encodeKeyString.contramap(ti =>
            ti.transactionId.toHex ++ "#" ++ ti.index.toString
        )

    given transactionInputKeyDecoder: KeyDecoder[TransactionInput] with {
        override def apply(s: String): Option[TransactionInput] =
            s.split("#").toList match {
                case txIdStr :: idxStr :: Nil =>
                    for {
                        txId <- Try(TransactionHash.fromHex(txIdStr)).toOption
                        int <- KeyDecoder
                            .decodeKeyInt(idxStr)
                        idx <- if int >= 0 then Some(int) else None
                    } yield TransactionInput(txId, idx)
                case _ => None
            }
    }

    given positiveIntEncoder: Encoder[PositiveInt] = Encoder.encodeInt.contramap(_.toInt)

    given positiveIntDecoder: Decoder[PositiveInt] = Decoder.instance { c =>
        for {
            int <- c.as[Int]
            value <- PositiveInt.apply(int) match {
                case None =>
                    Left(
                      io.circe.DecodingFailure(s"Expected a positive integer, got $int", c.history)
                    )
                case Some(pi) => Right(pi)
            }
        } yield value
    }
}

package hydrozoa.timingviz

import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.timingviz.Ids.*
import hydrozoa.timingviz.TimedObject.*

/** Projects `TimingVisualizerState` into a `Frame` consumable by the renderer. Pure; no IO. */
object Presentation:

    def render(s: TimingVisualizerState): Frame =
        val items = s.objects.values.toList.flatMap(itemsFor(_, s))
        Frame(
          nowMs = s.now.instant.toEpochMilli,
          config = renderConfig(s.config),
          tracks = groupByTrack(items),
          derivations = derivationsFor(s)
        )

    // --- helpers ---------------------------------------------------------------

    private def renderConfig(cfg: hydrozoa.config.head.multisig.timing.TxTiming): ConfigFrame =
        ConfigFrame(
          minSettlementMs = cfg.minSettlementDuration.convert.finiteDuration.toMillis,
          inactivityMarginMs = cfg.inactivityMarginDuration.convert.finiteDuration.toMillis,
          silenceMs = cfg.silenceDuration.convert.finiteDuration.toMillis,
          depositSubmissionMs = cfg.depositSubmissionDuration.convert.finiteDuration.toMillis,
          depositMaturityMs = cfg.depositMaturityDuration.convert.finiteDuration.toMillis,
          depositAbsorptionMs = cfg.depositAbsorptionDuration.convert.finiteDuration.toMillis
        )

    private def itemsFor(
        tagged: TaggedObject,
        s: TimingVisualizerState
    ): List[TrackItem] =
        val src = tagged.source
        tagged.timedObject match
            case Request(id, validity) =>
                val oid = ObjectId.OfRequest(id)
                List(
                  TrackItem.Bar(
                    id = s"request-${id.asString}",
                    objectId = oid,
                    label = s"request ${id.asString}",
                    kind = ItemKind.Request,
                    source = src,
                    startMs = ms(validity.start),
                    endMs = ms(validity.end)
                  )
                )

            case Block(id, creation, kind, forced) =>
                val oid = ObjectId.OfBlock(id)
                val label =
                    if forced then s"block ${id.asString} ($kind, forced)"
                    else s"block ${id.asString} ($kind)"
                List(
                  TrackItem.Bar(
                    id = s"block-${id.asString}",
                    objectId = oid,
                    label = label,
                    kind = ItemKind.BlockCreation,
                    source = src,
                    startMs = ms(creation.start),
                    endMs = ms(creation.end)
                  )
                )

            case SpineEffect(id, kind, validity, _) =>
                val oid = ObjectId.OfEffect(id)
                val itemKind = kind match
                    case SpineKind.Init         => ItemKind.SpineInit
                    case SpineKind.Settlement   => ItemKind.SpineSettlement
                    case SpineKind.Finalization => ItemKind.SpineFinalization
                List(
                  TrackItem.Bar(
                    id = s"spine-${id.asString}",
                    objectId = oid,
                    label = s"$kind ${id.asString}",
                    kind = itemKind,
                    source = src,
                    startMs = ms(validity.start),
                    endMs = ms(validity.end)
                  )
                )

            case Fallback(id, start, _) =>
                val oid = ObjectId.OfEffect(id)
                List(
                  TrackItem.Marker(
                    id = s"fallback-${id.asString}",
                    objectId = oid,
                    label = s"fallback ${id.asString}",
                    kind = ItemKind.Fallback,
                    source = src,
                    atMs = ms(start.convert)
                  )
                )

            case d @ Deposit(id, end, status) =>
                depositItems(
                  d,
                  oid = ObjectId.OfDeposit(id),
                  src = src,
                  end = end,
                  s = s,
                  status = status
                )

            case Refund(id, start, _) =>
                val oid = ObjectId.OfEffect(id)
                List(
                  TrackItem.Marker(
                    id = s"refund-${id.asString}",
                    objectId = oid,
                    label = s"refund ${id.asString}",
                    kind = ItemKind.Refund,
                    source = src,
                    atMs = ms(start)
                  )
                )

    private def depositItems(
        d: Deposit,
        oid: ObjectId,
        src: Source,
        end: RequestTimes.RequestValidityEndTime,
        status: DepositStatus,
        s: TimingVisualizerState
    ): List[TrackItem] =
        val cfg = s.config
        val submissionDeadline = cfg.depositSubmissionDeadline(end)
        val absorptionStart = cfg.depositAbsorptionStartTime(end)
        val absorptionEnd = cfg.depositAbsorptionEndTime(end)
        val refundStart = cfg.refundValidityStart(end)
        val didStr = d.id.asString
        val statusSuffix = status match
            case DepositStatus.Pending      => ""
            case DepositStatus.OnChain      => " [on-chain]"
            case DepositStatus.Mature       => " [mature]"
            case DepositStatus.Absorbed(by) => s" [absorbed by ${by.asString}]"
            case DepositStatus.Rejected(by) => s" [rejected by ${by.asString}]"
            case DepositStatus.Refunded     => " [refunded]"
        List(
          TrackItem.Bar(
            id = s"deposit-$didStr-submission",
            objectId = oid,
            label = s"deposit $didStr: submission$statusSuffix",
            kind = ItemKind.DepositSubmissionWindow,
            source = src,
            startMs = ms(end.convert),
            endMs = ms(submissionDeadline.convert)
          ),
          TrackItem.Bar(
            id = s"deposit-$didStr-maturity",
            objectId = oid,
            label = s"deposit $didStr: maturity wait",
            kind = ItemKind.DepositMaturityWait,
            source = src,
            startMs = ms(submissionDeadline.convert),
            endMs = ms(absorptionStart.convert)
          ),
          TrackItem.Bar(
            id = s"deposit-$didStr-absorption",
            objectId = oid,
            label = s"deposit $didStr: absorption window",
            kind = ItemKind.DepositAbsorptionWindow,
            source = src,
            startMs = ms(absorptionStart.convert),
            endMs = ms(absorptionEnd.convert)
          ),
          TrackItem.Bar(
            id = s"deposit-$didStr-silence",
            objectId = oid,
            label = s"deposit $didStr: silence",
            kind = ItemKind.DepositSilenceWindow,
            source = src,
            startMs = ms(absorptionEnd.convert),
            endMs = ms(refundStart.convert)
          ),
          TrackItem.Marker(
            id = s"deposit-$didStr-refund-start",
            objectId = oid,
            label = s"deposit $didStr: refund start",
            kind = ItemKind.DepositRefundStart,
            source = src,
            atMs = ms(refundStart.convert)
          )
        )

    private def groupByTrack(items: List[TrackItem]): TracksFrame =
        def kindToTrack(k: ItemKind): Track = k match
            case ItemKind.Request                 => Track.Requests
            case ItemKind.BlockCreation           => Track.Blocks
            case ItemKind.SpineInit               => Track.SpineEffects
            case ItemKind.SpineSettlement         => Track.SpineEffects
            case ItemKind.SpineFinalization       => Track.SpineEffects
            case ItemKind.Fallback                => Track.Fallbacks
            case ItemKind.Refund                  => Track.Refunds
            case ItemKind.DepositSubmissionWindow => Track.Deposits
            case ItemKind.DepositMaturityWait     => Track.Deposits
            case ItemKind.DepositAbsorptionWindow => Track.Deposits
            case ItemKind.DepositSilenceWindow    => Track.Deposits
            case ItemKind.DepositRefundStart      => Track.Deposits
        def itemKind(i: TrackItem): ItemKind = i match
            case b: TrackItem.Bar    => b.kind
            case m: TrackItem.Marker => m.kind
        val byTrack = items.groupBy(i => kindToTrack(itemKind(i)))
        TracksFrame(
          requests = byTrack.getOrElse(Track.Requests, Nil).sortBy(startOf),
          blocks = byTrack.getOrElse(Track.Blocks, Nil).sortBy(startOf),
          spineEffects = byTrack.getOrElse(Track.SpineEffects, Nil).sortBy(startOf),
          fallbacks = byTrack.getOrElse(Track.Fallbacks, Nil).sortBy(startOf),
          deposits = byTrack.getOrElse(Track.Deposits, Nil).sortBy(startOf),
          refunds = byTrack.getOrElse(Track.Refunds, Nil).sortBy(startOf)
        )

    private def startOf(i: TrackItem): Long = i match
        case b: TrackItem.Bar    => b.startMs
        case m: TrackItem.Marker => m.atMs

    /** Edges combine two sources: the state's `derivations` map (currently always empty — reserved
      * for future transition-side population) and edges we can compute by inspecting the spec
      * relationships between objects in the state.
      */
    private def derivationsFor(s: TimingVisualizerState): List[DerivationEdge] =
        val stored = s.derivations.toList.flatMap { case ((tgtObj, tgtField), sources) =>
            sources.toList.map((srcObj, srcField) =>
                DerivationEdge(tgtObj, tgtField, srcObj, srcField)
            )
        }
        stored ++ computedEdges(s)

    private def computedEdges(s: TimingVisualizerState): List[DerivationEdge] =
        val spinesByEffectId: Map[hydrozoa.timingviz.Ids.EffectId, ObjectId] =
            s.objects.toList.collect {
                case (oid @ ObjectId.OfEffect(_), TaggedObject(SpineEffect(eid, _, _, _), _)) =>
                    eid -> oid
            }.toMap

        val depositOidByDepositId: Map[hydrozoa.timingviz.Ids.DepositId, ObjectId] =
            s.objects.toList.collect {
                case (oid @ ObjectId.OfDeposit(_), TaggedObject(Deposit(did, _, _), _)) =>
                    did -> oid
            }.toMap

        s.objects.values.toList.flatMap { tagged =>
            tagged.timedObject match
                case Fallback(fallbackId, _, pairedEffectId) =>
                    // Spec: settlement.validity_end = paired_fallback.validity_start − silence
                    spinesByEffectId.get(pairedEffectId).toList.map { spineOid =>
                        DerivationEdge(
                          targetObject = spineOid,
                          targetField = FieldKey.ValidityEnd,
                          sourceObject = ObjectId.OfEffect(fallbackId),
                          sourceField = FieldKey.FallbackStart
                        )
                    }
                case Refund(refundId, _, depId) =>
                    // The refund's start is locked relative to the deposit's request-validity-end.
                    depositOidByDepositId.get(depId).toList.map { depOid =>
                        DerivationEdge(
                          targetObject = ObjectId.OfEffect(refundId),
                          targetField = FieldKey.RefundStart,
                          sourceObject = depOid,
                          sourceField = FieldKey.ValidityEnd
                        )
                    }
                case _ => Nil
        }

    private def ms(q: QuantizedInstant): Long = q.instant.toEpochMilli

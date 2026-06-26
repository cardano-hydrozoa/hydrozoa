// Mirror of the Scala wire types in hydrozoa.timingviz.{Frame, Command}.
// Keep this file in lockstep with Scala until/unless we cross-compile.

export type Source = "Observed" | "Hypothetical";

export type ItemKind =
    | "Request"
    | "BlockCreation"
    | "SpineInit"
    | "SpineSettlement"
    | "SpineFinalization"
    | "Fallback"
    | "Refund"
    | "DepositSubmissionWindow"
    | "DepositMaturityWait"
    | "DepositAbsorptionWindow"
    | "DepositSilenceWindow"
    | "DepositRefundStart";

export type BlockKind = "Initial" | "Minor" | "Major" | "Final";
export type SpineKind = "Init" | "Settlement" | "Finalization";

export type ObjectId =
    | { type: "OfBlock"; id: string }
    | { type: "OfRequest"; id: string }
    | { type: "OfDeposit"; id: string }
    | { type: "OfEffect"; id: string };

export type FieldKey =
    | "ValidityStart"
    | "ValidityEnd"
    | "CreationStart"
    | "CreationEnd"
    | "FallbackStart"
    | "RefundStart"
    | "AbsorptionStart"
    | "AbsorptionEnd"
    | "SubmissionDeadline"
    | "ForcedMajorWakeup";

export interface Bar {
    type: "Bar";
    id: string;
    objectId: ObjectId;
    label: string;
    kind: ItemKind;
    source: Source;
    startMs: number;
    endMs: number;
}

export interface Marker {
    type: "Marker";
    id: string;
    objectId: ObjectId;
    label: string;
    kind: ItemKind;
    source: Source;
    atMs: number;
}

export type TrackItem = Bar | Marker;

export interface TracksFrame {
    requests: TrackItem[];
    blocks: TrackItem[];
    spineEffects: TrackItem[];
    fallbacks: TrackItem[];
    deposits: TrackItem[];
    refunds: TrackItem[];
}

export interface ConfigFrame {
    minSettlementMs: number;
    inactivityMarginMs: number;
    silenceMs: number;
    depositSubmissionMs: number;
    depositMaturityMs: number;
    depositAbsorptionMs: number;
}

export interface DerivationEdge {
    targetObject: ObjectId;
    targetField: FieldKey;
    sourceObject: ObjectId;
    sourceField: FieldKey;
}

export interface Frame {
    nowMs: number;
    config: ConfigFrame;
    tracks: TracksFrame;
    derivations: DerivationEdge[];
}

export type ParameterKey =
    | "MinSettlementDuration"
    | "InactivityMarginDuration"
    | "SilenceDuration"
    | "DepositSubmissionDuration"
    | "DepositMaturityDuration"
    | "DepositAbsorptionDuration";

export type Command =
    | { type: "AdvanceClock"; toMs: number }
    | { type: "SetParameter"; key: ParameterKey; valueMs: number }
    | { type: "ObserveBlock"; id: string; creation: { startMs: number; endMs: number }; kind: BlockKind }
    | { type: "ObserveRequest"; id: string; validity: { startMs: number; endMs: number } }
    | { type: "ObserveDepositRequest"; id: string; endMs: number }
    | { type: "ObserveDepositOnChain"; id: string }
    | { type: "ObserveDepositAbsorbed"; id: string; by: string }
    | { type: "ObserveDepositRejected"; id: string; by: string }
    | {
          type: "ObserveSpineEffect";
          id: string;
          kind: SpineKind;
          blockId: string;
          validity: { startMs: number; endMs: number };
      }
    | { type: "ObserveFallback"; id: string; pairedEffectId: string; startMs: number }
    | { type: "ObserveRefund"; id: string; depositId: string; startMs: number }
    | { type: "HypothesizeDeposit"; id: string; endMs: number }
    | { type: "HypothesizeBlock"; id: string; creation: { startMs: number; endMs: number }; kind: BlockKind }
    | { type: "Retract"; id: ObjectId };

export const TRACKS: { key: keyof TracksFrame; label: string }[] = [
    { key: "requests", label: "Requests" },
    { key: "blocks", label: "Blocks" },
    { key: "spineEffects", label: "Spine effects" },
    { key: "fallbacks", label: "Fallbacks" },
    { key: "deposits", label: "Deposits" },
    { key: "refunds", label: "Refunds" },
];

export const objectIdKey = (oid: ObjectId): string => `${oid.type}:${oid.id}`;

export const itemAt = (item: TrackItem): number =>
    item.type === "Bar" ? item.startMs : item.atMs;

export const itemEnd = (item: TrackItem): number =>
    item.type === "Bar" ? item.endMs : item.atMs;

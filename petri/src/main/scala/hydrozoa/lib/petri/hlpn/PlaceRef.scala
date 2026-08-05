package hydrozoa.lib.petri.hlpn

/** A typed handle to a place under construction: the place's id carried with a phantom color type,
  * so wiring an arc can check its inscription's color against the place's *at compile time*. Erased
  * to the id at runtime — the type only exists to constrain [[NetBuilder.input]] /
  * [[NetBuilder.output]]. Minted only by the builder (the constructor is package-private), so a ref
  * cannot be forged with the wrong color.
  */
opaque type PlaceRef[PlaceId, C] = PlaceId

object PlaceRef:
    private[hlpn] def wrap[PlaceId, C](id: PlaceId): PlaceRef[PlaceId, C] = id
    extension [PlaceId, C](ref: PlaceRef[PlaceId, C]) def id: PlaceId = ref

/** A typed handle to a transition under construction. A transition binds several variables of
  * possibly different colors, so it carries no single color type.
  */
opaque type TransitionRef[TransitionId] = TransitionId

object TransitionRef:
    private[hlpn] def wrap[TransitionId](id: TransitionId): TransitionRef[TransitionId] = id
    extension [TransitionId](ref: TransitionRef[TransitionId]) def id: TransitionId = ref

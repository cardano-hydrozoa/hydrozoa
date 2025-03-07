package hydrozoa.l2.ledger

/** @tparam U
  *   type for representing utxo set
  * @tparam E
  *   type for events
  * @tparam H
  *   type for event hash
  * @tparam V
  *   type for event verifier
  */
trait L2Ledger[+U, E, H, -V]:
    def activeState: U
    def submit(event: L2Event): H
    def event(hash: H): Option[E]
    def allEvents: Set[H]
    def isEmpty: Boolean

/** @tparam E
  *   type for events
  */
trait Verifier[E]:
    def isValid(event: E): Boolean

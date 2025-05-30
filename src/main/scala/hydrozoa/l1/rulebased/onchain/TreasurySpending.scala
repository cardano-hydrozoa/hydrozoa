package hydrozoa.l1.rulebased.onchain

enum TreasuryRedeemer:
    case Resolve
    case Withdraw(withdrawRedeemer: WithdrawRedeemer)

case class WithdrawRedeemer(
    // TBD
)

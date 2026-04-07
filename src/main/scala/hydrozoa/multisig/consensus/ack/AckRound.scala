package hydrozoa.multisig.consensus.ack

sealed trait AckRound
trait AckRound1 extends AckRound
trait AckRound2 extends AckRound

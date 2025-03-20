package hydrozoa.l1

import hydrozoa.{TxAny, Network, TxId}
import scalus.ledger.api.v1.PosixTime

trait Cardano {
    def submit(tx: TxAny): Either[SubmissionError, TxId]
    def network: Network
    def lastBlockTime: PosixTime
}

type SubmissionError = String

package test

import org.scalacheck.util.Pretty
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyPaymentPart.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage

object PrettyGivens {

    given prettyUtxo: (Utxo => Pretty) = utxo =>
        Pretty(params => s"Utxo(${Pretty.pretty(utxo.input)},${Pretty.pretty(utxo.output)})")

    given prettyTxIn: (TransactionInput => Pretty) = ti =>
        Pretty(params =>
            val truncatedHash = ti.transactionId.toHex.take(6)
            s"$truncatedHash(...)#${ti.index}"
        )

    given prettyShelleyPP: (ShelleyPaymentPart => Pretty) = x =>
        Pretty(_ =>
            x match {
                case Key(k)                       => s"$Key(${k.toHex.take(6)}(...))"
                case ShelleyPaymentPart.Script(s) => s"Script${s.toHex.take(6)}(...)"
            }
        )

    given prettyShelleyAddress: (ShelleyAddress => Pretty) = x =>
        Pretty(_ =>
            s"ShelleyAddress(${Pretty.pretty(x.network)},${Pretty.pretty(x.payment)},${Pretty.pretty(x.delegation)}}"
        )

    given prettyAddress: (Address => Pretty) = a =>
        Pretty(_ =>
            a match {
                case sa: ShelleyAddress => Pretty.pretty(sa)
                case x                  => Pretty.pretty(x)
            }
        )

    given prettyBabbage: (Babbage => Pretty) = babbage =>
        Pretty(_ =>
            s"Babbage(${Pretty.pretty(babbage.address)}," +
                s" ${Pretty.pretty(babbage.value)}," +
                s"${Pretty.pretty(babbage.datumOption)}," +
                s"${Pretty.pretty(babbage.scriptRef)})"
        )

    given prettyOutput: (TransactionOutput => Pretty) = to =>
        Pretty(_ =>
            to match {
                case b: Babbage => Pretty.pretty(b)
                case to         => Pretty.pretty(to)
            }
        )
}

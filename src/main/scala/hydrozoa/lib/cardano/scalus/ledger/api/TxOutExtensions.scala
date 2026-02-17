package hydrozoa.lib.cardano.scalus.ledger.api

import io.bullet.borer.Encoder
import scalus.*
import scalus.uplc.builtin.{Data, FromData}
import scalus.cardano.ledger.TransactionOutput
import scalus.ledger.api.v2.OutputDatum.OutputDatum
import scalus.ledger.api.v3.TxOut

@Compile
object TxOutExtension {
    extension (self: TxOut)
        /** Returns inline datum of type T of fails.
          *
          * @param x$1
          * @tparam T
          * @return
          */
        def inlineDatumOfType[T](using FromData[T]): T =
            val OutputDatum(inlineDatum) = self.datum: @unchecked
            inlineDatum.to[T]
}

object TransactionOutputEncoders {
    given Encoder[TransactionOutput.Shelley] =
        summon[Encoder[TransactionOutput]].asInstanceOf[Encoder[TransactionOutput.Shelley]]

    given Encoder[TransactionOutput.Babbage] =
        summon[Encoder[TransactionOutput]].asInstanceOf[Encoder[TransactionOutput.Babbage]]
}

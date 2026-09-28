package hydrozoa.lib.cardano.scalus.cardano.onchain.plutus

import io.bullet.borer.Encoder
import scalus.cardano.ledger.TransactionOutput

object TransactionOutputEncoders {
    given Encoder[TransactionOutput.Shelley] =
        summon[Encoder[TransactionOutput]].asInstanceOf[Encoder[TransactionOutput.Shelley]]

    given Encoder[TransactionOutput.Babbage] =
        summon[Encoder[TransactionOutput]].asInstanceOf[Encoder[TransactionOutput.Babbage]]
}

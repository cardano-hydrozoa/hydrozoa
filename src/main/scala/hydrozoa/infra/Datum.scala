package hydrozoa.infra

import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.Datum
import scalus.builtin.ByteString

def datumByteString(datum: Datum): ByteString =
    ByteString.fromArray(datum.bytes)

def deserializeDatumHex(hex: String): Datum = Datum(HexUtil.decodeHexString(hex))

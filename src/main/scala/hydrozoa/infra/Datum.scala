package hydrozoa.infra

import hydrozoa.Datum
import scalus.builtin.ByteString

def datumByteString(datum: Datum): ByteString =
    ByteString.fromArray(datum.bytes)

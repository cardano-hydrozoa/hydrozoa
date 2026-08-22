package hydrozoa.bootstrap

import io.circe.parser.decode
import org.scalatest.funsuite.AnyFunSuite

/** [[Bootstrap.L2Output]]'s optional `evacuationKey`: the escape hatch that lets an opening L2
  * output be keyed the way a remote ledger keys it, instead of by the synthetic eutxo reference the
  * bootstrap derives from the seed utxo.
  */
class L2OutputEvacuationKeyTest extends AnyFunSuite:

    /** A SugarRush fee-account key: a 28-byte account hash zero-padded to 32. */
    private val remoteKeyHex =
        "e04159fa6693834ddb3dd21a1ce289929a2a2f0f66d67b904e76f84400000000"

    private val address =
        "addr_test1vrsyzk06v6fcxnwm8hfp588z3xff5230pandv7usfem0s3qvscar4"

    private def json(extra: String): String =
        s"""{"address":"$address","value":{"coin":"10000000","assets":{}}$extra}"""

    test("an entry carrying an evacuationKey decodes it") {
        val out = decode[Bootstrap.L2Output](json(s""","evacuationKey":"$remoteKeyHex""""))
            .fold(e => fail(s"decode failed: $e"), identity)
        assert(out.evacuationKey.map(_.byteString.toHex).contains(remoteKeyHex))
    }

    test("an entry without one decodes to None, so the synthetic eutxo key is used") {
        val out = decode[Bootstrap.L2Output](json(""))
            .fold(e => fail(s"decode failed: $e"), identity)
        assert(out.evacuationKey.isEmpty)
    }

    test("a malformed evacuationKey is rejected rather than silently ignored") {
        assert(decode[Bootstrap.L2Output](json(""","evacuationKey":"not-hex"""")).isLeft)
    }

    test("the address and value still decode alongside an explicit key") {
        val out = decode[Bootstrap.L2Output](json(s""","evacuationKey":"$remoteKeyHex""""))
            .fold(e => fail(s"decode failed: $e"), identity)
        assert(out.toTransactionOutput.value.coin.value == 10000000L)
    }

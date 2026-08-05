package hydrozoa.config.head.parameters

import io.circe.Json
import io.circe.syntax.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}

/** Codec pins for [[L2LedgerKind]]. The field is agreed by every peer and hashed into the head
  * parameters, so the exact kebab wire spelling is load-bearing: a regression that serialized the
  * enum's `.toString` (`CardanoEutxo`) instead of `cardano-eutxo` still round-trips structurally,
  * so only asserting the literal strings here catches it.
  */
object L2LedgerKindTest extends Properties("L2LedgerKind codec") {

    val _ = property("round-trips both variants") = forAll(Gen.oneOf(L2LedgerKind.values.toList)) {
        kind =>
            kind.asJson.as[L2LedgerKind] == Right(kind)
    }

    val _ = property("cardano-eutxo encodes to its kebab literal") =
        L2LedgerKind.CardanoEutxo.asJson == Json.fromString("cardano-eutxo")

    val _ = property("any-remote encodes to its kebab literal") =
        L2LedgerKind.AnyRemote.asJson == Json.fromString("any-remote")

    val _ = property("an unknown string is a decode error") =
        Json.fromString("CardanoEutxo").as[L2LedgerKind].isLeft &&
            Json.fromString("sugar-rush").as[L2LedgerKind].isLeft
}

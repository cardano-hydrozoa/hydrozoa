package hydrozoa.node.rest

/* Question (Peter, 2025-08-10): My assumption is that this is the file is where we define our serialization
boundary; i.e., where we go from an endpoint that contains a Bech32 string to a full-fledged ShelleyAddress.
But this may be achievable earlier with tapirs schemas? I'm not sure.
 */

import com.github.plokhotnyuk.jsoniter_scala.core.{
    JsonKeyCodec,
    JsonReader,
    JsonValueCodec,
    JsonWriter
}
import scalus.cardano.ledger.*
import scalus.cardano.address.Address as SAddress
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.*
import hydrozoa.l2.consensus.network.{*, given}
import hydrozoa.l2.ledger.{L2EventTransaction, L2EventWithdrawal}
import hydrozoa.node.rest.NodeRestApi.{
    depositEndpoint,
    finalizeEndpoint,
    initEndpoint,
    stateL2Endpoint,
    submitL1Endpoint,
    submitL2Endpoint
}
import hydrozoa.node.server.{
    DepositRequest,
    DepositResponse,
    Node,
    depositResponseCodec,
    depositResponseSchema
}
import hydrozoa.node.state.WalletId
import ox.channels.ActorRef
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import sttp.tapir.*
import sttp.tapir.CodecFormat.TextPlain
import sttp.tapir.generic.auto.schemaForCaseClass
import sttp.tapir.json.jsoniter.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import scala.util.{Try, Success, Failure}

/** Hydrozoa Node API, currently implemented in terms of Tapir HTTP server.
  */
class NodeRestApi(node: ActorRef[Node]):

    private val apiEndpoints =
        List(
          initEndpoint.handle(runInit),
          depositEndpoint.handle(runDeposit),
          submitL1Endpoint.handle(runSubmitL1),
          submitL2Endpoint.handle(runSubmitL2),
          stateL2Endpoint.handle(runStateL2),
          finalizeEndpoint.handle(runFinalize)
        )

    private val swaggerEndpoints = SwaggerInterpreter()
        .fromEndpoints[[X] =>> X](apiEndpoints.map(_.endpoint), "Hydrozoa Head API", "0.1")

    def mkServer(port: Int): NettySyncServer =
        NettySyncServer()
            .host("0.0.0.0")
            .port(port)
            .addEndpoints(apiEndpoints ++ swaggerEndpoints)

    private def runInit(request: InitRequest): Either[String, String] =
        node.ask(
          _.initializeHead(
            request.otherPeers.toSet,
            request.amount,
            request.seedUtxoTxId,
            request.seedUtxoTxIx
          ).map(_.toHex)
        )

    private def runDeposit(
        txId: String,
        txIx: Long,
        depositAmount: BigInt,
        deadline: Option[BigInt],
        address: String,
        datum: Option[String],
        refundAddress: String,
        refundDatum: Option[String]
    ): Either[String, DepositResponse] =
        node.ask(
          _.deposit(
            DepositRequest(
              (TransactionHash.fromHex(txId)),
              TxIx(txIx.toInt),
              depositAmount,
              deadline,
              Address.unsafeFromBech32(address),
              (datum match
                  case None => None
                  case Some(s) =>
                      if s.isEmpty then None
                      else {
                          Some(Data.fromCbor(ByteString.fromHex(s)))
                      }
              ),
              Address.unsafeFromBech32(refundAddress),
              (refundDatum match
                  case None => None
                  case Some(s) =>
                      if s.isEmpty then None else Some(Data.fromCbor(ByteString.fromHex(s)))
              )
            )
          )
        )

    private def runSubmitL1(tx: String): Either[String, String] =
        node.ask(_.submitL1(tx).map(_.toString))

    private def runSubmitL2(req: SubmitRequestL2): Either[String, String] =
        node.ask(_.submitL2(req).map(_.toString))

    private def runStateL2(_unit: Unit): Either[Unit, StateL2Response] =
        Right(node.ask(_.stateL2()))

    private def runFinalize(_unit: Unit): Either[String, String] =
        node.ask(_.tryFinalize())

object NodeRestApi:
    val initEndpoint = endpoint.put
        .in("init")
        .in(jsonBody[InitRequest])
        .out(stringBody)
        .errorOut(stringBody)

    /** Simplified API for depositing. */
    val depositEndpoint = endpoint.put
        .in("deposit")
        .in(query[String]("txId"))
        .in(query[Long]("txIx"))
        .in(query[BigInt]("depositAmount"))
        .in(query[Option[BigInt]]("deadline"))
        .in(query[String]("address"))
        .in(query[Option[String]]("datum"))
        .in(query[String]("refundAddress"))
        .in(query[Option[String]]("refundDatum"))
        .out(jsonBody[DepositResponse])
        .errorOut(stringBody)

    val submitL1Endpoint = endpoint.put
        .in("l1")
        .in("submit")
        .in(stringBody)
        .out(stringBody)
        .errorOut(stringBody)

    val submitL2Endpoint = endpoint.put
        .in("l2")
        .in("submit")
        .in(jsonBody[SubmitRequestL2])
        .out(stringBody)
        .errorOut(stringBody)

    val stateL2Endpoint = endpoint.get
        .in("l2")
        .in("state")
        .out(jsonBody[StateL2Response])

    val finalizeEndpoint = endpoint.post.in("finalize").out(stringBody).errorOut(stringBody)

// JSON/Schema instances
enum SubmitRequestL2:
    case Transaction(transaction: L2EventTransaction)
    case Withdrawal(withdrawal: L2EventWithdrawal)

object SubmitRequestL2:
    def apply(event: L2EventTransaction | L2EventWithdrawal): SubmitRequestL2 =
        event match
            case tx: L2EventTransaction => Transaction(tx)
            case wd: L2EventWithdrawal  => Withdrawal(wd)

given submitRequestL2Codec: JsonValueCodec[SubmitRequestL2] =
    JsonCodecMaker.make

given submitRequestL2Schema: Schema[SubmitRequestL2] =
    Schema.binary[SubmitRequestL2]

def decode(s: String): DecodeResult[TransactionHash] =
    Try(TransactionHash.fromHex(s)) match {
        case Success(s) => DecodeResult.Value(s)
        case Failure(f) => DecodeResult.Error(s, f)
    }

def encode(txId: TransactionHash): String = txId.toHex

given Codec[String, TransactionHash, TextPlain] =
    Codec.string.mapDecode(decode)(encode)

type StateL2Response = List[(UtxoId[L2], OutputNoTokens[L2])]

//given policyIdCodec: JsonValueCodec[PolicyId] =
//    JsonCodecMaker.make
//
//given tokenNameCodec: JsonValueCodec[TokenName] =
//    JsonCodecMaker.make

//implicit val policyIdCodec: JsonKeyCodec[PolicyId] = new JsonKeyCodec[PolicyId] {
//    override def decodeKey(in: JsonReader): PolicyId = PolicyId(in.readKeyAsString())
//
//    override def encodeKey(x: PolicyId, out: JsonWriter): Unit =
//        out.writeKey(x.policyId)
//}
//
//implicit val tokenNameCodec: JsonKeyCodec[TokenName] = new JsonKeyCodec[TokenName] {
//    override def decodeKey(in: JsonReader): TokenName = TokenName(in.readKeyAsString())
//
//    override def encodeKey(x: TokenName, out: JsonWriter): Unit =
//        out.writeKey(x.tokenName)
//}

given stateL2ResponseCodec: JsonValueCodec[StateL2Response] =
    ??? // FIXME: JsonCodecMaker.make

given tokenNameSchema: Schema[AssetName] =
    Schema.derived[AssetName]

// FIXME (2025-07-04): A dependency update caused this to fail due to ambiguous
//     given instances. It was strictly necessary at the time, so we commented it out.
//given stateL2ResponseSchema: Schema[StateL2Response] =
//    Schema.derived[StateL2Response]

case class InitRequest(
    otherPeers: List[WalletId],
    amount: Long,
    seedUtxoTxId: TransactionHash,
    seedUtxoTxIx: TxIx
)

given initRequestCodec: JsonValueCodec[InitRequest] =
    JsonCodecMaker.make

given initRequestSchema: Schema[InitRequest] =
    Schema.derived[InitRequest]

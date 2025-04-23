package hydrozoa.node.rest

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.*
import hydrozoa.infra.deserializeDatumHex
import hydrozoa.l2.ledger.{SimpleTransaction, SimpleWithdrawal}
import hydrozoa.node.rest.NodeRestApi.{
    depositEndpoint,
    initEndpoint,
    stateL2Endpoint,
    submitL1Endpoint,
    submitL2Endpoint,
    finalizeEndpoint
}
import hydrozoa.node.server.{
    DepositRequest,
    DepositResponse,
    Node,
    depositResponseCodec,
    depositResponseSchema
}
import ox.channels.ActorRef
import sttp.tapir.*
import sttp.tapir.generic.auto.schemaForCaseClass
import sttp.tapir.json.jsoniter.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/** Hydrozoa Node API, currently backed by Tapir HTTP server.
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

    private def runInit(amount: Long, txId: String, txIx: Long): Either[String, String] =
        node.ask(_.initializeHead(amount, TxId(txId), TxIx(txIx.toChar)).map(_.hash))

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
              TxId(txId),
              TxIx(txIx.toChar),
              depositAmount,
              deadline,
              AddressBechL2(address),
              (datum match
                  case None    => None
                  case Some(s) => if s.isEmpty then None else Some(deserializeDatumHex(s))
              ),
              AddressBechL1(refundAddress),
              (refundDatum match
                  case None    => None
                  case Some(s) => if s.isEmpty then None else Some(deserializeDatumHex(s))
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
        .in(query[Long]("amount")) // how much ADA should be deposited for fees into the treasury
        .in(query[String]("txId"))
        .in(query[Long]("txIx"))
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
    case Transaction(transaction: SimpleTransaction)
    case Withdrawal(withdrawal: SimpleWithdrawal)

object SubmitRequestL2:
    def apply(event: SimpleTransaction | SimpleWithdrawal): SubmitRequestL2 =
        event match
            case tx: SimpleTransaction => Transaction(tx)
            case wd: SimpleWithdrawal  => Withdrawal(wd)

given submitRequestL2Codec: JsonValueCodec[SubmitRequestL2] =
    JsonCodecMaker.make

given submitRequestL2Schema: Schema[SubmitRequestL2] =
    Schema.derived[SubmitRequestL2]

given simpleTransactionSchema: Schema[SimpleTransaction] =
    Schema.derived[SimpleTransaction]

given simpleTWithdrawalSchema: Schema[SimpleWithdrawal] =
    Schema.derived[SimpleWithdrawal]

given txIdSchema: Schema[TxId] =
    Schema.derived[TxId]

given txIx: Schema[TxIx] =
    Schema.derived[TxIx]

type StateL2Response = List[(UtxoId[L2], Output[L2])]

given stateL2ResponseCodec: JsonValueCodec[StateL2Response] =
    JsonCodecMaker.make

given stateL2ResponseSchema: Schema[StateL2Response] =
    Schema.derived[StateL2Response]

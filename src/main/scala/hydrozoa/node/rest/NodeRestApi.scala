package hydrozoa.node.rest

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.*
import hydrozoa.infra.deserializeDatumHex
import hydrozoa.l2.ledger.{SimpleTransaction, SimpleWithdrawal}
import hydrozoa.node.TestPeer.{Bob, Carol, mkWalletId}
import hydrozoa.node.server.{DepositRequest, Node}
import sttp.tapir.*
import sttp.tapir.generic.auto.schemaForCaseClass
import sttp.tapir.json.jsoniter.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import scala.concurrent.duration.{FiniteDuration, SECONDS}

/** Hydrozoa Node API, currently backed by Tapir HTTP server.
  */
class NodeRestApi(node: Node):

    private val initEndpoint = endpoint.put
        .in("init")
        .in(query[Long]("amount")) // how much ADA should be deposited for fees into the treasury
        .in(query[String]("txId"))
        .in(query[Long]("txIx"))
        .out(stringBody)
        .errorOut(stringBody)
        .handle(runInitializeHead)

    /** Simplified API for depositing. */
    private val depositEndpoint = endpoint.put
        .in("deposit")
        .in(query[String]("txId"))
        .in(query[Long]("txIx"))
        .in(query[Option[BigInt]]("deadline"))
        .in(query[String]("address"))
        .in(query[Option[String]]("datum"))
        .in(query[String]("refundAddress"))
        .in(query[Option[String]]("refundDatum"))
        .out(stringBody)
        .errorOut(stringBody)
        .handle(runDeposit)

    private val submitL1Endpoint = endpoint.put
        .in("l1")
        .in("submit")
        .in(stringBody)
        .out(stringBody)
        .errorOut(stringBody)
        .handle(submitL1)

    private val submitL2Endpoint = endpoint.put
        .in("l2")
        .in("submit")
        .in(jsonBody[SubmitRequestL2])
        .out(stringBody)
        .errorOut(stringBody)
        .handle(submitL2)

    private val nextBlockEndpoint =
        endpoint.post
            .in("l2")
            .in("next")
            .in(query[Option[String]]("nextBlockFinal"))
            .out(stringBody)
            .errorOut(stringBody)
            .handle(nextBlock)

    private val apiEndpoints =
        List(initEndpoint, depositEndpoint, submitL1Endpoint, submitL2Endpoint, nextBlockEndpoint)

    private val swaggerEndpoints = SwaggerInterpreter()
        .fromEndpoints[[X] =>> X](apiEndpoints.map(_.endpoint), "Hydrozoa Head API", "0.1")

    def start(): Unit =
        NettySyncServer()
            .port(8088)
            .modifyConfig(c => c.connectionTimeout(FiniteDuration(1200, SECONDS)))
            .addEndpoints(apiEndpoints ++ swaggerEndpoints)
            .startAndWait()

    private def runInitializeHead(amount: Long, txId: String, txIx: Long): Either[String, String] =
        val defPeers = Set(Bob, Carol).map(mkWalletId)
        node.initializeHead(defPeers, amount, TxId(txId), TxIx(txIx.toChar)).map(_.hash)

    private def runDeposit(
        txId: String,
        txIx: Long,
        deadline: Option[BigInt],
        address: String,
        datum: Option[String],
        refundAddress: String,
        refundDatum: Option[String]
    ): Either[String, String] =
        node.deposit(
          DepositRequest(
            TxId(txId),
            TxIx(txIx.toChar),
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
        ).map(_.toString)

    private def submitL1(tx: String): Either[String, String] =
        node.submitL1(tx).map(_.toString)

    private def submitL2(req: SubmitRequestL2): Either[String, String] =
        node.submitL2(req).map(_.toString)

    private def nextBlock(nextBlockFinal: Option[String]): Either[String, String] =
        val b = nextBlockFinal match
            case Some(_) => true
            case None    => false
        node.handleNextBlock(b).map(_.toString)

// JSON/Schema instances
enum SubmitRequestL2:
    case Transaction(transaction: SimpleTransaction)
    case Withdrawal(withdrawal: SimpleWithdrawal)

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

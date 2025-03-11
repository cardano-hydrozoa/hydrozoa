package hydrozoa.node.api

import hydrozoa.*
import hydrozoa.infra.deserializeDatumHex
import hydrozoa.node.server.{DepositRequest, Node}
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import scala.concurrent.duration.{FiniteDuration, SECONDS}

/** Hydrozoa Node API, currently backed by Tapir HTTP server.
  */
class NodeApi(node: Node):

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

    private val majorEndpoint =
        endpoint.post
            .in("l2")
            .in("major")
            .in(query[Option[String]]("nextBlockFinal"))
            .out(stringBody)
            .errorOut(stringBody)
            .handle(major)

    private val apiEndpoints = List(initEndpoint, depositEndpoint, submitL1Endpoint, majorEndpoint)

    private val swaggerEndpoints = SwaggerInterpreter()
        .fromEndpoints[[X] =>> X](apiEndpoints.map(_.endpoint), "Hydrozoa Head API", "0.1")

    def start(): Unit =
        NettySyncServer()
            .port(8088)
            .modifyConfig(c => c.connectionTimeout(FiniteDuration(1200, SECONDS)))
            .addEndpoints(apiEndpoints ++ swaggerEndpoints)
            .startAndWait()

    private def runInitializeHead(amount: Long, txId: String, txIx: Long): Either[String, String] =
        node.initializeHead(amount, TxId(txId), TxIx(txIx)).map(_.hash)

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
            TxIx(txIx),
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
        node.submit(tx).map(_.toString)

    private def major(nextBlockFinal: Option[String]): Either[String, String] =
        val b = nextBlockFinal match
            case Some(_) => true
            case None    => false
        node.handleNextMajorBlock(b)

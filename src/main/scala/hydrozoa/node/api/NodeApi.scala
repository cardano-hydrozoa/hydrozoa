package hydrozoa.node.api

import hydrozoa.*
import hydrozoa.node.server.{DepositRequest, Node}
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.swagger.bundle.SwaggerInterpreter

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
        .in(query[String]("address"))
        .in(query[String]("datum"))
        .in(query[String]("refundAddress"))
        .in(query[String]("refundDatum"))
        .out(stringBody)
        .errorOut(stringBody)
        .handle(runDeposit)

    private val apiEndpoints = List(initEndpoint, depositEndpoint)

    private val swaggerEndpoints = SwaggerInterpreter()
        .fromEndpoints[[X] =>> X](apiEndpoints.map(_.endpoint), "Hydrozoa Head API", "0.1")

    def start(): Unit =
        NettySyncServer()
            .port(8088)
            .addEndpoints(apiEndpoints ++ swaggerEndpoints)
            .startAndWait()

    private def runInitializeHead(amount: Long, txId: String, txIx: Long): Either[String, String] =
        node.initializeHead(amount, TxId(txId), TxIx(txIx)).map(_.hash)

    private def runDeposit(
        txId: String,
        txIx: Long,
        address: String,
        datum: String,
        refundAddress: String,
        refundDatum: String
    ): Either[String, String] =
        node.deposit(
          DepositRequest(
            TxId(txId),
            TxIx(txIx),
            AddressBechL2(address),
            Datum(???),
            AddressBechL1(refundAddress),
            Datum(???)
          )
        ).map(_.toString)

package hydrozoa.head

import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/**
 * Hydrozoa Node API, currently backed by Tapir HTTP server.
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
  private val apiEndpoints = List(initEndpoint)
  private val swaggerEndpoints = SwaggerInterpreter()
    .fromEndpoints[[X] =>> X](apiEndpoints.map(_.endpoint), "Init Head endpoint", "0.1")

  def start(): Unit =
    NettySyncServer()
      .port(8088)
      .addEndpoints(apiEndpoints ++ swaggerEndpoints)
      .startAndWait()

  private def runInitializeHead(amount: Long, txId: String, txIx: Long): Either[String, String] =
    node.initializeHead(amount, TxId(txId), TxIx(txIx))

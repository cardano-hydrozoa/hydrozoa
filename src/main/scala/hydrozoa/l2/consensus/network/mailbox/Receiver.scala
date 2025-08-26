package hydrozoa.l2.consensus.network.mailbox

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.mailbox.BatchMsgOrMatchIndexMsg.{
    CaseBatchMsg,
    CaseMatchIndexMsg
}
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.*
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.*
import io.netty.handler.codec.http.websocketx.*
import io.netty.handler.stream.ChunkedWriteHandler
import ox.*
import ox.channels.*
import ox.scheduling.{RepeatConfig, repeat}

import scala.annotation.nowarn
import scala.util.Try

/** Likely, not an actor but something else that physically receives messages from multiple remote
  * [[TransmitterActor]]s and passes them to the local [[InboxActor]] or [[OutboxActor]].
  */
abstract class Receiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor]):

    /** An incoming request from a peer telling us to add new messages to our inbox */
    final def handleAppendEntries(batchMsg: BatchMsg[Inbox]): Unit =
        inboxActor.tell(_.appendEntries(batchMsg._1, batchMsg._2))

    /** An incoming request from a peer, indicating the highest message THEY have processed */
    final def handleConfirmMatchIndex(matchIndexMsg: MatchIndexMsg[Outbox]): Unit =
        outboxActor.tell(_.confirmMatchIndex(matchIndexMsg._1, matchIndexMsg._2): Unit)

/** A short-circuit receiver.
  *
  * @param outboxActor
  * @param inboxActor
  */
final class LocalReceiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor])
    extends Receiver(outboxActor, inboxActor)

/** This receiver starts Netty server that serves `ws://0.0.0.0:<port>/ws` endpoint.
  *
  * @param outboxActor
  * @param inboxActor
  * @param port
  * @param ox
  */
class WSReceiver(
    outboxActor: ActorRef[OutboxActor],
    inboxActor: ActorRef[InboxActor],
    port: Int
)(using ox: Ox)
    extends Receiver(outboxActor, inboxActor):

    private val log = Logger(getClass)

    @nowarn
    val bossGroup = NioEventLoopGroup()
    @nowarn
    val workerGroup = NioEventLoopGroup()
//    val bossGroup = NioIoHandler.newFactory()
//    val workerGroup = NioIoHandler.newFactory()

    case class WebSocketFrameHandler() extends SimpleChannelInboundHandler[WebSocketFrame]:
        override def channelRead0(ctx: ChannelHandlerContext, frame: WebSocketFrame): Unit =
            frame match
                case textFrame: TextWebSocketFrame =>
                    val text = textFrame.text()
                    log.debug(s"Received: $text")
                    Try(
                      readFromString(text)(using batchMsgOrMatchIndexMsgCodec[Inbox])
                    ).toEither match {
                        case Right(matchIndexMsg) =>
                            matchIndexMsg match {
                                case CaseBatchMsg(batchMsg) => handleAppendEntries(batchMsg)
                                case CaseMatchIndexMsg(matchIndexMsg) =>
                                    // FIXME: I just sidestep typechecker here for now, sorry...
                                    handleConfirmMatchIndex(
                                      matchIndexMsg.asInstanceOf[MatchIndexMsg[Outbox]]
                                    )
                            }
                        case Left(ex) =>
                            log.error(s"Failed to parse incoming message: ${ex.getMessage}")
                    }
                case _: CloseWebSocketFrame =>
                    log.warn("Received close frame")
                    ctx.close(): Unit
                case _ =>
                    log.warn("Received unknown frame")
                    ()

    forkDiscard(
      repeat(RepeatConfig.immediateForever()) {
          log.info(s"Starting server at ws://localhost:$port/ws")
          try
              val bootstrap = ServerBootstrap()
                  // TODO: do we need two groups?
                  .group(bossGroup, workerGroup)
                  .channel(classOf[NioServerSocketChannel])
                  .childHandler(
                    new ChannelInitializer[SocketChannel]():
                        override def initChannel(ch: SocketChannel): Unit =
                            val pipeline = ch.pipeline()
                            pipeline.addLast(HttpServerCodec())
                            pipeline.addLast(HttpObjectAggregator(65536))
                            pipeline.addLast(ChunkedWriteHandler())
                            pipeline.addLast(new WebSocketServerProtocolHandler("/ws"))
                            pipeline.addLast(WebSocketFrameHandler())
                            ()
                  )
              val channel = bootstrap.bind(port).sync().channel()
              log.info(s"WebSocket server started at ws://localhost:$port/ws")
              channel.closeFuture().sync()
              ()
          finally
              // TODO: do we need to wait these futures to be completed?
              bossGroup.shutdownGracefully()
              workerGroup.shutdownGracefully()
              ()
      }
    )

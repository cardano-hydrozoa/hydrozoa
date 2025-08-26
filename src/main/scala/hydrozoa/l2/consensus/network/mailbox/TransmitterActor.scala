package hydrozoa.l2.consensus.network.mailbox

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.mailbox.BatchMsgOrMatchIndexMsg.{
    CaseBatchMsg,
    CaseMatchIndexMsg
}
import io.netty.bootstrap.Bootstrap
import io.netty.channel.*
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.*
import io.netty.handler.codec.http.websocketx.*
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import ox.*
import ox.channels.{Actor, ActorRef}

import java.net.URI
import scala.annotation.nowarn
import scala.collection.mutable
import scala.concurrent.duration.*

/** Physically sends a batch of messages to a particular peer.
  *
  * Planned implementations:
  *   - LocalTransmitterActor
  *   - SymmetricWSTransmitterActor (remote)
  */
trait TransmitterActor:

    /** Called by node's [[OutboxActor]] when it wants to send a [[batch]] for [[to]] peer.
      *
      * Implementations should execute the actual sending in the background and return immediately
      * to start working on the following actor's inbox messages.
      *
      * @param to
      *   the recipient node
      * @param batch
      *   the messages may be empty
      * @return
      *   Must return a Left-throwable if transmission is unsuccessful; otherwise unit.
      */
    def appendEntries(to: PeerId, batch: Batch[Outbox]): Either[Throwable, Unit]

    /** Called by node's [[InboxActor]] when it wants to confirm [[matchIndex]] for [[to]] peer.
      *
      * Implementations should execute the actual sending in the background and return immediately
      * to start working on the following actor's inbox messages.
      *
      * @param to
      *   the recipient node
      * @param matchIndex
      *   the current matchIndex for [[to]] peer in the local [[InboxActor]]
      * @return
      *   Must return a Left-throwable if transmission is unsuccessful; otherwise unit.
      */
    def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex[Inbox]): Either[Throwable, Unit]

/** Transmits messages locally (for testing) directly using ox actors
  */
final class LocalTransmitterActor(myself: PeerId) extends TransmitterActor:

    private val log = Logger(getClass)

    val peers: mutable.Map[PeerId, Receiver] = mutable.Map.empty

    /** Replicate messages in out outbox to the peer's inbox */
    override def appendEntries(
        to: PeerId,
        batch: Batch[Outbox]
    ): Either[LocalTransmitterError, Unit] = {

        // The batch we send is OUR outbox, but must be received at the PEERS inbox
        val inBatch = Batch
            .fromList[Inbox](
              batch.toList.map(msg => MailboxMsg[Inbox](MsgId[Inbox](msg.id.toLong), msg.content))
            )
            .get
        peers.get(to) match {
            case None => {
                log.error(
                  s"[${myself.asString}] transmission failed: appendEntries(${to}, ${batch})"
                )
                Left(LocalTransmitterError.PeerNotFound)
            }
            case Some(peer) => {
                peer.handleAppendEntries(myself, inBatch)
                log.debug(s"[${myself.asString}] appendEntries to: $to, batch: $batch")
                Right(())
            }
        }
    }

    /** Confirm to the peer the highest message id that WE have processed from the peer */
    override def confirmMatchIndex(
        to: PeerId,
        matchIndex: MatchIndex[Inbox]
    ): Either[LocalTransmitterError, Unit] =

        // The index we send is the highest index WE have processed for the remote peer. This is reflected in THEIR
        // outbox.
        val outIndex = MatchIndex[Outbox](matchIndex.toLong)
        peers.get(to) match {
            case None => {
                log.error(
                  s"[${myself.asString}] transmission failed for confirmMatchIndex(${to}, ${matchIndex})"
                )
                Left(LocalTransmitterError.PeerNotFound)
            }
            case Some(peer) => {
                peer.handleConfirmMatchIndex(myself, outIndex)
                log.debug(
                  s"[${myself.asString}] confirmMatchIndex to: $to, matchIndex: $matchIndex"
                )
                Right(())
            }
        }

    def connect(to: PeerId, receiver: Receiver): Unit = peers.put(to, receiver): Unit

    def disconnect(to: PeerId): Unit = peers.remove(to): Unit

/** TODO: pass all peers in the constructor
  *
  * @param myself
  * @param ox
  */
class WSTransmitterActor(myself: PeerId)(using ox: Ox) extends TransmitterActor:

    private val log = Logger(getClass)

    @nowarn
    private val group = NioEventLoopGroup()
    private val channels: mutable.Map[PeerId, Channel] = mutable.Map.empty

    private var self: ActorRef[WSTransmitterActor] = _

    override def appendEntries(to: PeerId, batch: Batch[Outbox]): Either[Throwable, Unit] = {
        channels.get(to) match {
            case Some(channel) if channel.isActive =>
                val msg = CaseBatchMsg(myself, batch)
                val serMsg = writeToString(msg)(using batchMsgOrMatchIndexMsgCodec)
                channel.writeAndFlush(new TextWebSocketFrame(serMsg)): Unit
            case _ =>
                log.warn(s"Trying to send a batch to $to, but channel is not active, skipping.")
        }
        Right(())
    }

    override def confirmMatchIndex(
        to: PeerId,
        matchIndex: MatchIndex[Inbox]
    ): Either[Throwable, Unit] = {
        channels.get(to) match {
            case Some(channel) if channel.isActive =>
                val msg = CaseMatchIndexMsg(myself, matchIndex)
                val serMsg = writeToString(msg)(using batchMsgOrMatchIndexMsgCodec)
                channel.writeAndFlush(new TextWebSocketFrame(serMsg)): Unit
            case _ =>
                log.warn(
                  s"Trying to send a match index to $to, but channel is not active, skipping."
                )
        }
        Right(())
    }

    /** Connect to another server.
      *
      * TODO: Currently, blocks the actor I believe.
      *
      * @param to
      * @param uri
      */
    def connect(to: PeerId, uri: URI): Unit = {

        def newBootstrap() = Bootstrap()
            .group(group)
            .channel(classOf[NioSocketChannel])
            // TODO check other options
            .option(ChannelOption.SO_KEEPALIVE, Boolean.box(true))

        def createReconnectHandler(handshaker: WebSocketClientHandshaker) =

            def reconnect(): Unit = {
                println("Connection lost, reconnecting in 3s...")
                forkDiscard {
                    sleep(3.seconds)
                    self.tell(_.connect(to, uri))
                }
            }

            WebSocketClientHandler(handshaker, () => reconnect())

        class WebSocketClientHandler(handshaker: WebSocketClientHandshaker, onClose: () => Unit)
            extends SimpleChannelInboundHandler[Object] {

            @volatile var handshakeFuture: ChannelPromise = _

            override def handlerAdded(ctx: ChannelHandlerContext): Unit =
                handshakeFuture = ctx.newPromise()

            override def channelActive(ctx: ChannelHandlerContext): Unit =
                handshaker.handshake(ctx.channel()): Unit

            override def channelInactive(ctx: ChannelHandlerContext): Unit = {
                println("Channel inactive")
                onClose()
            }

            override def channelRead0(ctx: ChannelHandlerContext, msg: Object): Unit = {
                val ch = ctx.channel()
                if (!handshaker.isHandshakeComplete) {
                    handshaker.finishHandshake(ch, msg.asInstanceOf[FullHttpResponse])
                    handshakeFuture.setSuccess()
                    return
                }
                msg match {
                    case frame: TextWebSocketFrame => println(s"Received: ${frame.text()}")
                    case _: CloseWebSocketFrame    => ch.close(): Unit
                    case _                         => // ignore
                }
            }
        }

        // connect itself

        log.info(s"connecting to peer $to at $uri")

        val scheme = uri.getScheme
        val host = uri.getHost
        val port = if (uri.getPort == -1) if (scheme == "wss") 443 else 80 else uri.getPort

        val mbSslCtx =
            if scheme == "wss" then
                Some(
                  SslContextBuilder
                      .forClient()
                      .trustManager(InsecureTrustManagerFactory.INSTANCE)
                      .build()
                )
            else None

        val handshaker = WebSocketClientHandshakerFactory.newHandshaker(
          uri,
          WebSocketVersion.V13,
          null,
          true,
          EmptyHttpHeaders.INSTANCE
        )

        val reconnectHandler = createReconnectHandler(handshaker)

        val bootstrap = newBootstrap().handler(new ChannelInitializer[SocketChannel] {
            override def initChannel(ch: SocketChannel): Unit = {
                val p = ch.pipeline()
                mbSslCtx.foreach(ctx => p.addLast(ctx.newHandler(ch.alloc(), host, port)))
                p.addLast(new HttpClientCodec())
                p.addLast(new HttpObjectAggregator(8192))
                p.addLast(reconnectHandler): Unit
            }
        })

        try {
            val ch = bootstrap.connect(host, port).sync().channel()
            reconnectHandler.handshakeFuture.sync()
            log.info(s"Connected to peer $to successfully")
            channels.put(to, ch): Unit
        } catch {
            case ex: Exception =>
                log.error(s"Connection to $to failed: ${ex.getMessage}, retrying in 3s...")
                forkDiscard {
                    sleep(3.seconds)
                    self.tell(_.connect(to, uri))
                }
        }
    }

// def disconnect(to: PeerId): Unit = peers.remove(to): Unit

object WSTransmitterActor:
    def create(myself: PeerId)(using ox: Ox): ActorRef[TransmitterActor] =
        val logic = WSTransmitterActor(myself)
        val actor = Actor.create(logic)
        logic.self = actor
        actor.asInstanceOf[ActorRef[TransmitterActor]]

enum LocalTransmitterError extends Throwable:
    /** Returned when the peer is not found in the peers map */
    case PeerNotFound

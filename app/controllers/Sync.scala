package controllers

import akka.{Done, NotUsed}
import akka.stream.Materializer
import akka.stream.alpakka.amqp.{AmqpCredentials, AmqpDetailsConnectionProvider, AmqpLocalConnectionProvider, AmqpUriConnectionProvider, AmqpWriteSettings, NamedQueueSourceSettings, QueueDeclaration, ReadResult, WriteMessage, WriteResult}
import akka.stream.alpakka.amqp.scaladsl.{AmqpFlow, AmqpSource, CommittableReadResult}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.sql.{SQLException, Timestamp}
import java.util.NoSuchElementException
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import com.google.inject.AbstractModule
import com.google.inject.name.Names
import com.rabbitmq.client.{ConnectionFactory, DeliverCallback}

/*

TODO: поменять данные в БД чтобы там не встречалась кириллица
либо посмотреть как менять кодировку чтобы в раббит можно было запихивать кириллицу


 */

class SyncModule extends AbstractModule {
  override def configure(): Unit = bind(classOf[Sync]).asEagerSingleton()
}

class Sync @Inject() (
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment
)(implicit mat: Materializer) extends HasDatabaseConfigProvider[JdbcProfile] {
  import environment._

  object AMQP {
    val Host = "localhost"
    val ReadQueue = "read_queue"
    val WriteQueue = "write_queue"
  }

  val amqpUri = "amqp://guest:guest@127.0.0.1:5672"
  val defaultParallelism = 1
  val connectionProvider = AmqpLocalConnectionProvider
  val readQueueDeclaration = QueueDeclaration("read_queue").withDurable(true)

  val amqpSourceRead: Source[ReadResult, NotUsed] = AmqpSource.atMostOnceSource(
    NamedQueueSourceSettings(connectionProvider, "read_queue")
      .withDeclaration(readQueueDeclaration)
      .withAckRequired(false),
    bufferSize = 20
  )

  val readResult: Future[_] = amqpSourceRead
    .mapAsync(defaultParallelism) {
      cm =>
        val message = new String(cm.bytes.toArray)

        val fResult = for {
          _ <- processMessage(message)
        } yield Done

        fResult recoverWith {
          ex =>
            sendExToAmqp(message, ex)
        }
    }
    .runWith(Sink.seq)


  def processMessage(message: String): Future[NotUsed] = {


    object Action {
      val insert = "insert"
      val update = "update"
      val write = "write"
      val read = "read"
    }

    val fParam = Future {
      val jMessage = Json.parse(message)
      val messageTemplate = (jMessage \ "template").as[String]
      val messageAction = (jMessage \ "action").as[String]
      val messageBody = (jMessage \ "body").as[JsObject]
      (messageTemplate, messageAction, messageBody)
    }

    def fProcessMessage(action: String, body: JsObject, template: String): Future[Any] = action match {
      case Action.insert =>
        db run EntityTemplateProcessor.insert(body, template)
      case Action.update =>
        db run EntityTemplateProcessor.update(body, template)
      case Action.write =>
        db run (EntityTemplateProcessor.delete(body, template) >> EntityTemplateProcessor.insert(body, template)).transactionally
      case Action.read =>
        db run EntityTemplateProcessor.select(body, template) map {
          r =>
            sendToAmqp(Json.toJson(r).toString)
        }
    }

    val fRes = for {
      (templateName, action, body) <- fParam
      template <- fGetEntityTemplate(templateName)
      _ <- fProcessMessage(action, body, template)
    } yield NotUsed

    fRes recover {
      case _ => NotUsed
    }
  }

  private val writeQueueDeclaration = QueueDeclaration(AMQP.WriteQueue).withDurable(true)

  private[this] val writeSettings = AmqpWriteSettings(connectionProvider)
    .withRoutingKey(AMQP.WriteQueue)
    .withDeclaration(writeQueueDeclaration)

  private[this] val writeAmqpFlow: Flow[WriteMessage, WriteResult, Future[Done]] =
    AmqpFlow.withConfirm(writeSettings)

  def sendToAmqp(message: String): Future[Seq[WriteResult]] = {
    Source(Seq(message))
      .map(m => WriteMessage(ByteString(m)))
      .via(writeAmqpFlow)
      .runWith(Sink.seq)
  }

  def sendExToAmqp(message: String, ex: Throwable): Future[Seq[WriteResult]] =
    sendToAmqp(
      Json.obj(
        "type" -> "error",
        "exception" -> ex.toString,
        "message" -> message
      ).toString
    )
}

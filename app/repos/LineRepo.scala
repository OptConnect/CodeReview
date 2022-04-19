package repos

import akka.actor.ActorSystem
import models.Line
import org.slf4j.LoggerFactory
import play.api.libs.json.Json.obj
import play.modules.reactivemongo.{NamedDatabase, ReactiveMongoApi}
import reactivemongo.api.Cursor
import reactivemongo.api.bson.{BSONDocumentReader, BSONDocumentWriter, BSONObjectID}
import reactivemongo.play.json.compat.json2bson.toDocumentWriter
import reactivemongo.api.bson.collection.BSONCollection
import reactivemongo.play.json.compat._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class LineRepo @Inject()(implicit
                         ec: ExecutionContext,
                         @NamedDatabase("midway-admin") val reactiveMongoApi: ReactiveMongoApi,
                         actorSystem: ActorSystem)
  extends Repository[Line]()(
    ec, reactiveMongoApi,
    actorSystem) {

  val collectionName = "lines"
  val logger = LoggerFactory.getLogger(getClass)

  def findByNsDeviceId(deviceId: Long): Future[Seq[Line]] = collection() { col =>
    col.find(obj("netsuiteDetails.deviceId" -> deviceId))
      .cursor[Line]().collect[Seq](defaultMaxDocs, Cursor.FailOnError[Seq[Line]]())
  }

}

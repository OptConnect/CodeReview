package repos

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import play.api.libs.json.Json._
import play.api.libs.json._
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.akkastream.{State, cursorProducer}
import reactivemongo.api.Cursor
import reactivemongo.api.commands.{Command, CommandWithResult, WriteResult}

import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.api.bson.{BSONDocumentReader, BSONDocumentWriter, BSONObjectID}
import reactivemongo.play.json.compat.json2bson.toDocumentWriter
import reactivemongo.api.bson.collection.BSONCollection
import reactivemongo.play.json.compat._
import models.SearchResult

abstract class Repository[T](implicit ec: ExecutionContext, reactiveMongoApi: ReactiveMongoApi, actorSystem: ActorSystem) {

  val defaultMaxDocs = 300
  val collectionName:String

  object Ping extends Command with CommandWithResult[Boolean]
  implicit val pingCommandWriter: BSONDocumentWriter[Ping.type] = BSONDocumentWriter[Ping.type] { _: Ping.type => obj("ping" -> 1) }
  implicit val pingResultReader: BSONDocumentReader[Boolean] = BSONDocumentReader.option[Boolean] { _.booleanLike("ok") }

  def runPing()(implicit ec: ExecutionContext): Future[Boolean] = reactiveMongoApi.database.flatMap(_.runCommand(Ping))

  def defaultErrorHandler[T] = Cursor.FailOnError[T]()


  def findById(_id:BSONObjectID)(implicit reads:BSONDocumentReader[T]) =
    collection()(_.find(obj("_id" -> _id)).one[T])

  def findOneByCriteria(criteria:JsObject)(implicit reads:BSONDocumentReader[T]) =
    collection()(_.find(criteria).one[T])

  def findByCriteria(criteria:JsObject, sortCriteria:JsObject)(implicit reads:BSONDocumentReader[T]) =
    collect(maxDocs = defaultMaxDocs)(_.find(criteria).sort(sortCriteria).cursor[T]())

  def findByCriteria(criteria:JsObject, maxDocs:Int = defaultMaxDocs)(implicit reads:BSONDocumentReader[T]): Future[Seq[T]] =
    collect(maxDocs = maxDocs)(_.find(criteria).cursor[T]())

  def findByCriteria(criteria:JsObject, sortCriteria:JsObject, maxDocs:Int)(implicit reads:BSONDocumentReader[T]): Future[Seq[T]] =
    collect(maxDocs = maxDocs)(_.find(criteria).sort(sortCriteria).cursor[T]())

  def countOf(criteria:Option[JsObject] = None): Future[Long] = {
    collection()(_.count(criteria.map(toDocument)))
  }

  def searchByCriteria(criteria:JsObject, sortByOption:Option[JsObject], page:Option[Int], resultsPerPage:Int = 15)(implicit reads:BSONDocumentReader[T]) = collection() { col =>
    val currentPage = page.getOrElse(1)
    val skip = if (currentPage > 0) (currentPage - 1) * resultsPerPage else 0
    val sortBy = sortByOption.getOrElse(obj("_id" -> -1))
    for {
      total <- col.count(Some(criteria))
      firstPage <- col.find(criteria)
        .sort(sortBy)
        .skip(skip)
        .cursor[T]()
        .collect[Seq](resultsPerPage, Cursor.FailOnError[Seq[T]]())
    } yield {
      SearchResult(currentPage, resultsPerPage, total, firstPage)
    }
  }

  def insert(doc:T)(implicit writes:BSONDocumentWriter[T]): Future[WriteResult] = collection()(_.insert.one[T](doc))

  def updateById(_id:BSONObjectID, doc:T)(implicit writes:BSONDocumentWriter[T]): Future[WriteResult] = collection()(
    _.update.one(q = Json.obj("_id" -> _id), u = doc)
  )

  def update(selector:JsObject, doc:T)(implicit writes:BSONDocumentWriter[T]): Future[WriteResult] = collection()(_.update.one(q = selector, u = doc))

  def updateFields(selector:JsObject, fields:JsObject, upsert:Boolean = false, multi:Boolean = false)(implicit writes:BSONDocumentWriter[T]): Future[WriteResult] = collection()(_.update.one(q = selector, u = fields, upsert = upsert, multi = multi))

  def getCollection(name:String):Future[BSONCollection] = reactiveMongoApi.database.map(_.collection(name))

  def collection[D](col:String = collectionName)(action:BSONCollection => Future[D]):Future[D] =
    getCollection(col).flatMap(action)

  def streaming[D](col:String = collectionName)(action:BSONCollection => Source[D, Future[State]]):Source[D, Future[Future[State]]] =
    Source.futureSource(getCollection(col).map(action))

  def streaming(filter:JsObject, sortCriteria:JsObject)(implicit reads:BSONDocumentReader[T]): Source[T, Future[Future[State]]] = streaming() { col =>
    col.find(filter).sort(sortCriteria).cursor[T]().documentSource()
  }

  def collect(collectionName:String = collectionName, maxDocs:Int = defaultMaxDocs)(action:BSONCollection => Cursor[T]) = {
    collection(collectionName) { col =>
      action(col).collect[Seq](maxDocs, Cursor.FailOnError[Seq[T]]())
    }
  }

  def remove(filter: JsObject) = collection() { col =>
    col.delete.one(q = filter)
  }

  def distinct(field:String, criteria:JsObject ):Future[List[Long]] = collection() { col =>
    col.distinct[Long, List](field, Some(criteria))
  }

  def bulkUpsert(updates:Seq[(JsObject, JsObject)]) =
    if (updates.nonEmpty)
      doBulkUpdate(updates, upsert = true)
    else
      Future.successful(None)

  def bulkUpdate(updates:Seq[(JsObject, JsObject)]) =
    if (updates.nonEmpty)
      doBulkUpdate(updates, upsert = false)
    else Future.successful(None)

  def bulkInsert(items:Seq[T])(implicit writes:OWrites[T]) = collection() { col =>
    val insert = col.insert(ordered = false)
    insert.many[T](items)
  }

  def doBulkUpdate(updates:Seq[(JsObject, JsObject)], upsert:Boolean = false) = collection() { col =>
    val update = col.update(false)

    val elements = updates.map { case (q, u) =>
      update.element(
        q = q,
        u = makeFieldSparse("$setOnInsert", makeFieldSparse("$set", u)),
        multi = false,
        upsert = upsert
      )
    }

    Future.sequence(elements).flatMap(ops => update.many(ops))
  }.map(Some(_))

  /**
   * Removes any children of the fieldName that are null. This is used to prevent adding unused or empty fields into the db
   *
   * Ex: calling (fieldName = "$set") on the following JsObject will remove the name field, but leave the date field
   * {
   *   "$set": {
   *     "date": "2018-02-19",
   *     "name": null
   *   }
   * }
   */
  private def makeFieldSparse(fieldName:String, obj:JsObject) = {
    (obj \ fieldName).asOpt[JsObject]
      .map(field => JsObject(field.fields.filter(_._2 != JsNull)))
      .map(sparseField => obj + (fieldName -> sparseField))
      .getOrElse(obj)
  }
}

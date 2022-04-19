package controllers

import models.{Line, SearchResult, ServicePlan}
import org.joda.time.DateTime

import javax.inject._
import play.api._
import play.api.libs.json.{JodaReads, JodaWrites, Json}
import play.api.mvc._
import reactivemongo.api.bson.BSONObjectID
import repos.LineRepo

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LineController @Inject()(
  val controllerComponents: ControllerComponents,
  val lineRepo: LineRepo,
)(implicit val ec: ExecutionContext) extends BaseController {

  import JodaReads._
  import JodaWrites._
  import reactivemongo.play.json.compat.bson2json.{fromReader, fromWriter}
  import reactivemongo.play.json.compat.json2bson.{toDocumentReader, toDocumentWriter}
  implicit val servicePlanFormat = ServicePlan.jsonWrites
  implicit val lineWrites = Json.writes[Line]
  implicit val writes = Json.writes[SearchResult[Line]]

  def findLineById(lineId: BSONObjectID) = Action.async {
    lineRepo.findById(lineId)
      .map(line => Ok(Json.toJson(line)))
  }

  def findDeviceByLineId(lineId: BSONObjectID) = Action.async {
    lineRepo.findById(lineId)
      .flatMap {
        case Some(line) =>
          line.netsuiteDetails.flatMap(_.deviceId).map(lineRepo.findByNsDeviceId)
            .getOrElse(default = Future.successful(Seq(line)))
        case None => Future.successful(Seq.empty)
      }
      .map(line => Ok(Json.toJson(line)))
  }

  def findLinesByActivationDate(startDate: Option[DateTime]) = Action.async {
    lineRepo.findByActivationDate(startDate.map(date => date.toString()))
      .flatMap { line =>
        var out = List[Line]()
        for (l <- line) {
          if (l.carrierDetails.lastActivated.isDefined && l.carrierDetails.lastActivated.get.isAfter(startDate.get)) {
            out = out ++ Seq(l)
          }
        }
        Future.successful(out)
      }
      .map(line => Ok(Json.toJson(line)))
  }



}

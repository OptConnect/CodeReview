package models

import org.joda.time.{DateTime, DateTimeZone, LocalDate}
import play.api.libs.json._
import squants.information.{InformationConversions => _, _}
import squants.information.InformationConversions._
import reactivemongo.api.bson.BSONObjectID

object PlanAlertType extends Enumeration {
  val Usage, Autosuspend = Value
  implicit val reads = Reads.enumNameReads(PlanAlertType)
  implicit val writes = Writes.enumNameWrites
}

object PlanAlertStatus extends Enumeration {
  val UnProcessed, Processed, Cancelled = Value
  implicit val reads = Reads.enumNameReads(PlanAlertStatus)
  implicit val writes = Writes.enumNameWrites
}

case class AlertAcknowledgement(
                                 dateTime:DateTime,
                                 acknowledgedByUsername: String
                               )

object AlertAcknowledgement {
  import JodaReads._
  import JodaWrites._
  implicit val format = Json.format[AlertAcknowledgement]
}

case class PlanAlert(
                      _id: BSONObjectID = BSONObjectID.generate(),
                      servicePlan: ServicePlan,
                      alertType:PlanAlertType.Value,
                      status: Option[PlanAlertStatus.Value] = Some(PlanAlertStatus.UnProcessed),
                      thresholdInMb:Long,
                      threshold: Threshold,
                      actualUsageInBytes:Long,
                      devices: Seq[PlanAlert.Device],
                      customerId:Option[Long],
                      billingCycleStartDate: LocalDate,
                      created:DateTime = new DateTime(DateTimeZone.UTC),
                      lastUpdated:DateTime = new DateTime(DateTimeZone.UTC),
                      notificationsSent:Boolean = false,
                      deferredSuspension:Option[Boolean] = Some(false),
                      acknowledgementDetails:Option[AlertAcknowledgement] = None
                    ) {

  def description = if (alertType == PlanAlertType.Usage)
    "Usage Alert: Usage Exceeded " + formattedThreshold
  else
    "Auto-suspend Alert: Usage Exceeded " + formattedThreshold

  def formattedActualUsage = PlanAlert.inAppropriateUnits(actualUsageInBytes.bytes)

  def formattedThreshold = PlanAlert.inAppropriateUnits(thresholdInMb.mebibytes)

  def formattedDateRange(datePattern: String) = {
    val alertStartDate = billingCycleStartDate
    val alertEndDate = alertStartDate.plusMonths(1).minusDays(1)
    val today = new LocalDate(DateTimeZone.UTC)
    val startDate = alertStartDate.toString(datePattern)
    val endDate = if (alertEndDate.isBefore(today)) alertEndDate.toString(datePattern) else "present"
    s"$startDate - $endDate"
  }
}

object PlanAlert {
  import JodaReads._
  import JodaWrites._
  import reactivemongo.play.json.compat.bson2json.{fromReader, fromWriter}
  implicit val servicePlanWrites = ServicePlan.mongoWrites
  implicit val servicePlaReads = ServicePlan.reads

  case class Line(
                   lineId: BSONObjectID,
                   nsLineId: Option[Long],
                   deviceId: DeviceId,
                   carrier: Carrier,
                   usage: Long,
                 ) {
    override def toString: String = {
      val id = identifier
      s"${id._1}: ${id._2}"
    }
    def identifier: (String, String) = {
      "Device ID" -> s"$carrier ${deviceId.kind} - ${deviceId._id}"
    }
  }

  case class Device(
                     lines: Seq[PlanAlert.Line],
                     usage: Long,
                     nsDeviceId: Option[Long],
                     serialNumber: Option[String],
                   ) {
    override def toString: String = {
      val id = identifier
      s"${id._1}: ${id._2}"
    }
    def identifier: (String, String) = {
      serialNumber.map(sn => "Serial Number" -> sn).getOrElse(
        "Device ID(s)" -> lines.map(_.identifier._2).mkString("; ")
      )
    }
  }

  implicit val alertLineFormat = Json.format[PlanAlert.Line]
  implicit val alertDeviceFormat = Json.format[PlanAlert.Device]
  implicit val format = Json.format[PlanAlert]

  val printFormat = "%,.2f"
  def inAppropriateUnits(information:Information): String = {
    if (information.in(Gibibytes).value.toInt > 0) printFormat.format(information.in(Gibibytes).value) + " GB"
    else if (information.in(Mebibytes).value.toInt > 0) printFormat.format(information.in(Mebibytes).value) + " MB"
    else printFormat.format(information.in(Kibibytes)) + " KB"
  }
}

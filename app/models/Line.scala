package models

import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import play.api.libs.json.{JodaReads, JodaWrites, Json, Reads, Writes}
import reactivemongo.api.bson.BSONObjectID

object LineStatus extends Enumeration {
  val Active = Value  // TS: "active"; ATT: "ACTIVATED_NAME"; Kore:"Active"
  val Deactivated = Value // TS: "deactive", ATT:"DEACTIVATED_NAME", Kore:"Deactivated"
  val PendingActivate = Value
  val Suspended = Value // TS: "suspend", "pending suspend"; ATT suspend flag; KORE:"Suspended"
  val PendingSuspend = Value
  val PendingRestore = Value
  val Other = Value
  val Unknown = Value

  def statusFor(s: String) = s match {
    case "pending activation" => PendingActivate
    case "active" => Active
    case "Active" => Active
    case "ACTIVATED_NAME" => Active
    case "Activated" => Active
    case "deactive" => Deactivated
    case "DEACTIVATED_NAME" => Deactivated
    case "Deactivated" => Deactivated
    case "suspend" => Suspended
    case "pending suspend" => PendingSuspend
    case "pending resume" => PendingRestore
    case "Suspended" => Suspended
    case _ => Other
  }

  def withoutPending(s:LineStatus.Value) = s match {
    case PendingActivate => Active
    case PendingRestore => Active
    case PendingSuspend => Suspended
    case _ => s
  }

  implicit val reads = Reads.enumNameReads(LineStatus)
  implicit val writes = Writes.enumNameWrites
}

case class Line (
  _id:BSONObjectID = BSONObjectID.generate(),
  carrier:Carrier,
  ocpServicePlan:Option[ServicePlan] = None,
  lineStatus: Option[LineStatus.Value] = None,
  netsuiteDetails: Option[NetsuiteDetails] = None,
  carrierDetails: CarrierDetails,
  mdnZipCode: Option[String] = None,       // verizon only?
  addedInMidway:Option[DateTime] = None,
  lastSyncWithCarrier:Option[DateTime] = None,
  lastSyncWithNetsuite:Option[DateTime] = None,
  lastUsageSync:Option[DateTime] = None,
  lineGroups: Option[Seq[BSONObjectID]] = None,
  tags: Option[Seq[String]] = None
) {
  def carrierDeviceId: Option[DeviceId] = {
    Line.carrierDeviceIdFrom(carrierDetails.deviceIds)
  }

  def deviceIdOfType(deviceIdType: DeviceIdType.Value) =
    netsuiteDetails.flatMap(_.deviceIds.find(_.kind == deviceIdType))
      .orElse(carrierDetails.deviceIds.find(_.kind == deviceIdType))

  def isMultiSim = netsuiteDetails.flatMap(_.isMultiSim).getOrElse(false)
}

object Line {

  val logger = LoggerFactory.getLogger(getClass)
  val numeric = """^\d+$""".r
  val carrierIntegrationDeviceIdTypes = Seq("meid", "esn", "iccid")
  val netsuiteProvidedDeviceIdTypes = Seq("meid", "esn", "iccid", "imei")

  import JodaReads._
  import JodaWrites._
  import reactivemongo.play.json.compat.bson2json.{fromReader, fromWriter}
  implicit val servicePlanWrites = ServicePlan.mongoWrites
  implicit val servicePlaReads = ServicePlan.reads
  implicit val format = Json.using[Json.WithDefaultValues].format[Line]

  def carrierDeviceIdFrom(deviceIds:Seq[DeviceId]):Option[DeviceId] = {
    val iccId = deviceIds.find(_.kind == DeviceIdType.iccid)
    val meid = deviceIds.find(_.kind == DeviceIdType.meid)
    val esn = deviceIds.find(_.kind == DeviceIdType.esn)
    //    val imei = deviceIds.find(_.kind == DeviceIdType.imei)
    iccId.orElse(meid).orElse(esn).orElse(deviceIds.headOption)
  }

}



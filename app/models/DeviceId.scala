package models

import play.api.libs.json._

object DeviceIdType extends Enumeration {
  val imei = Value
  val iccid = Value
  val meid = Value
  val esn = Value
  val mdn = Value
  val min = Value
  val msisdn = Value
  val imsi = Value
  val eid = Value

  implicit val reads = Reads.enumNameReads(DeviceIdType)
  implicit val writew = Writes.enumNameWrites

  def forName(kind:String) = values.find(v => v.toString == kind.toLowerCase())
  def forNameEither(kind:String) = forName(kind)
    .toRight(s"""Failed to parse `kind` with value $kind. Expected one of the following: ${values.toList.mkString(",")}""")

  val typesFromNetsuite = Seq(imei, meid, iccid, esn)

  def comesFromNetsuite(kind: DeviceIdType.Value) = typesFromNetsuite.contains(kind)

  val keyDeviceIdTypes = Seq(meid, esn, iccid)
}

case class DeviceId(_id:String, kind:DeviceIdType.Value) {
  override def toString: String = {
    kind.toString + ":" + _id
  }
}

object DeviceId {
  implicit val deviceIdFormat: OFormat[DeviceId] = Json.using[Json.WithDefaultValues].format[DeviceId]

  def validate(id: String, kind:DeviceIdType.Value): Either[String, DeviceId] = {
    kind match {
      case DeviceIdType.iccid if id.length != 19 && id.length != 20 => Left(s"iccid $id must be either 19 or 20 digits")
      case DeviceIdType.imei if id.length != 15 && id.length != 16 => Left(s"imei $id must be either 15 or 16 digits")
      case DeviceIdType.meid if id.length != 14 => Left(s"meid $id must be 14 digits")
      case DeviceIdType.esn if id.length != 8 && id.length != 11 => Left(s"esn $id must be either 8 or 11 digits")
      case _ => Right(DeviceId(id, kind))
    }
  }

  val netsuiteReads = new Reads[DeviceId]() {
    override def reads(json: JsValue): JsResult[DeviceId] = {
      val deviceId = for {
        id <- (json \ "id").asOpt[String]
        kind <- (json \ "type").asOpt[String].flatMap(k => DeviceIdType.forName(if (k == "sim") "iccid" else k))
      } yield {
        DeviceId(id, kind)
      }
      deviceId.map(JsSuccess(_)).getOrElse(JsError("Could not parse DeviceId from " + json))
    }
  }

  val netsuiteWrites = new Writes[DeviceId] {
    override def writes(o: DeviceId): JsValue = {
      Json.obj(
        "id" -> o._id,
        "type" -> o.kind.toString
      )
    }
  }

  val netsuiteEmaReads = new Reads[DeviceId]() {
    override def reads(json: JsValue): JsResult[DeviceId] = {
      val deviceId = for {
        id <- (json \ "id").asOpt[String]
        kind <- (json \ "kind").asOpt[String].flatMap(k => DeviceIdType.forName(if (k == "sim") "iccid" else k))
        if (id != "")
      } yield DeviceId(id, kind)

      deviceId.map(JsSuccess(_)).getOrElse(JsError("Could not parse DeviceId from " + json))
    }
  }

  val netsuiteEmaWrites = new Writes[DeviceId] {
    override def writes(o: DeviceId): JsValue = {
      Json.obj(
        "id" -> o._id,
        "kind" -> o.kind.toString
      )
    }
  }

  val thingSpaceReads = new Reads[DeviceId]() {
    override def reads(json: JsValue): JsResult[DeviceId] = {
      val deviceId = for {
        id <- (json \ "id").asOpt[String]
        kind <- (json \ "kind").asOpt[String]
      } yield {
        DeviceId(id, DeviceIdType.withName(kind.toLowerCase()))
      }
      deviceId.map(JsSuccess(_)).getOrElse(JsError("Could not parse DeviceId from " + json))
    }
  }

  val wsBodyWrites = new Writes[DeviceId] {
    override def writes(o: DeviceId): JsValue = {
      Json.obj(
        "id" -> o._id,
        "kind" -> o.kind.toString
      )
    }
  }
}

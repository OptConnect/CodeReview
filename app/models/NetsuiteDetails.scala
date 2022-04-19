package models

import play.api.libs.json.Json

case class NetsuiteDetails (
  @deprecated("Use device ID instead", "05/23/2019")
  netsuiteId:         Long,
  customerId:         Option[Long] = None,
  billingDay:         Option[Long] = None,
  customerName:       Option[String] = None,
  serialNumber:       Option[String] = None,
  deviceManufacturer: Option[String] = None,
  model:              Option[String] = None,
  staticIp:           Option[String] = None,
  mac:                Option[String] = None,
  deviceId:           Option[Long] = None,
  moduleId:           Option[Long] = None,
  lineId:             Option[Long] = None,
  isMultiSim:         Option[Boolean] = None,
  isByod:             Boolean = false,
  deviceIds:          Seq[DeviceId] = Nil,

  /**
  * Set to true when this device should be considered part of a pool. Device pooling sets a custom threshold for the
  * device based on the dataLimit field. The new threshold is dataLimit * number of devices with the same data limit.
  * So if this device has a data limit of 1GB and there are 8 other devices with 1GB as the limit, then the threshold
  * is 9GB total.
  */
  dataPooled:         Boolean = false,
  dataLimit:          Option[Long] = None,
)

object NetsuiteDetails {
  implicit val format = Json.format[NetsuiteDetails]
}

package models

import org.joda.time.DateTime

case class CarrierDetails (
                            carrierPlan:Option[String] = None,
                            carrierPlanStatus:Option[String] = None,
                            carrierAccount:Option[String] = None,
                            deviceIds:Seq[DeviceId] = Nil,
                            ipAddress: Option[String] = None,
                            created: Option[DateTime] = None,
                            lastActivated: Option[DateTime] = None
                          ) {
  def deviceIdOfType(deviceIdType: DeviceIdType.Value): Option[DeviceId] = deviceIds.find(_.kind == deviceIdType)
}

case class CarrierDetailsUpdate(
                                 deviceId:DeviceId,
                                 carrier: Carrier,
                                 carrierDetails:CarrierDetails
                               ) {
  val lineStatus = carrierDetails.carrierPlanStatus.map(LineStatus.statusFor)
}

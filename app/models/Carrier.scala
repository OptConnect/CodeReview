package models

import org.joda.time.{DateTimeZone, LocalDate}
import play.api.libs.json._

sealed trait Carrier {
  def billingCycleStartDayOfMonth:Int
  def code: String

  def currentBillingCycleStartDate: LocalDate = {
    def today = new LocalDate(DateTimeZone.UTC)
    billingCycleStartDate(today)
  }

  def billingCycleStartDate(dayInCycle: LocalDate): LocalDate = {
    val billingCycleDateInMonth = dayInCycle.withDayOfMonth(billingCycleStartDayOfMonth)
    if (billingCycleDateInMonth.isAfter(dayInCycle))
      billingCycleDateInMonth.minusMonths(1)
    else
      billingCycleDateInMonth
  }
}

object Carrier {

  implicit val reads = new Reads[Carrier] {
    override def reads(json: JsValue): JsResult[Carrier] = {
      JsSuccess(
        json.as[String] match {
          case "VERIZON" => VERIZON
          case "ATT" => ATT
          case "KORE" => KORE
        }
      )
    }
  }

  def forName(name: String): Option[Carrier] = name match {
    case "VERIZON" => Some(VERIZON)
    case "ATT" => Some(ATT)
    case "KORE" => Some(KORE)
    case _ => None
  }

  def forNameEither(name: String): Either[String, Carrier] = forName(name)
    .toRight(s"""Failed to parse `carrier` with name '$name'. Expected one of the following: 'VERIZON','ATT','KORE'""")

  implicit val jsonWrites: Writes[Carrier] = new Writes[Carrier] {
    override def writes(o: Carrier): JsValue = {
      JsString(o.toString)
    }
  }

  implicit def jsWrites[T <: Carrier](implicit ev : T <:< Carrier) : Writes[T] = jsonWrites.asInstanceOf[Writes[T]]

  case object VERIZON extends Carrier {
    val billingCycleStartDayOfMonth: Int = 2
    val code = "VERIZON"
  }

  case object KORE extends Carrier {
    val billingCycleStartDayOfMonth: Int = 24
    val code = "KORE"
  }

  case object ATT extends Carrier  {
    val billingCycleStartDayOfMonth: Int = 19
    val code = "ATT"
  }

}

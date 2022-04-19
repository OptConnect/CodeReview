package models

import play.api.libs.json._
import squants.information.{InformationConversions => _, _}
import squants.information.InformationConversions._

case class Threshold(
                      value:Information,
                      thresholdType:PlanAlertType.Value = PlanAlertType.Usage,
                      deferOnNonBusinessDays:Boolean = false
                    ) {
  def description: String =
    if (value > 1.gibibytes) value.toGibibytes.toLong + " GB"
    else if (value > 1.mebibytes) value.toMebibytes.toLong + " MB"
    else if (value > 1.kibibytes) value.toKibibytes.toLong + " KB"
    else value.toBytes.toLong + " Bytes"
}

object Threshold {

  implicit val writes = new Writes[Threshold] {
    override def writes(o: Threshold): JsValue = {
      Json.obj(
        "thresholdType" -> o.thresholdType,
        "threshold" -> o.value.toMebibytes.toLong,
        "deferOnNonBusinessDays" -> o.deferOnNonBusinessDays
      )
    }
  }

  implicit val reads = new Reads[Threshold] {
    override def reads(json: JsValue): JsResult[Threshold] = {
      val result = for {
        value <- (json \ "threshold").asOpt[Long].map(_.mebibytes)
        thresholdType <- (json \ "thresholdType").asOpt[PlanAlertType.Value]
        deferOnNonBusinessDays <- (json \ "deferOnNonBusinessDays").asOpt[Boolean]

      } yield Threshold(value, thresholdType, deferOnNonBusinessDays)
      result.map(JsSuccess(_)).getOrElse(JsError("Unable to parse " + json + " as Threshold"))
    }
  }
}
sealed trait ServicePlan {
  def name: String
  def code: String

  def thresholds:Seq[Threshold]

  import ServicePlan._

  def maxUsageNotificationThresholdExceeded(usageInBytes:Long): Option[Threshold] = {

    val matchingThresholds: Seq[Threshold] = thresholds
      .filter(t => t.value <= usageInBytes.bytes)

    if (matchingThresholds.isEmpty) None
    else Some(matchingThresholds.maxBy(_.value))
  }
}

object ServicePlan {

  val globalDefaultPlan = OcpStandardUsagePlan

  implicit val reads = new Reads[ServicePlan] {
    override def reads(json: JsValue): JsResult[ServicePlan] = {
      JsSuccess(
        forCode(json.as[String])
      )
    }
  }

  def forCode(code:String): ServicePlan = {
    code match {
      case "ocp_standard_usage" => ServicePlan.OcpStandardUsagePlan
      case "oc_20gb_usage" => ServicePlan.Oc20GbUsagePlan
      case "ocp_extended_usage" => ServicePlan.OcpExtendedUsagePlan
      case "ocp_test_usage" => ServicePlan.OcpTestUsagePlan
      case "early_alert_usage" => ServicePlan.EarlyAlertUsagePlan
      case _ => ServicePlan.OcpNoAlertPlan
    }
  }

  val jsonWrites = new Writes[ServicePlan] {
    override def writes(o: ServicePlan): JsValue = {
      Json.obj(
        "name" -> o.name,
        "code" -> o.code,
        "thresholds" -> o.thresholds
      )
    }
  }

  val mongoWrites = new Writes[ServicePlan] {
    override def writes(o: ServicePlan): JsValue = JsString(o.code)
  }

  case object OcpStandardUsagePlan extends ServicePlan {
    val name = "OC Standard Usage"
    val code = "ocp_standard_usage"
    val thresholds = Seq(
      Threshold(4.gibibytes),
      Threshold(8.gibibytes),
      Threshold(10.gibibytes, PlanAlertType.Autosuspend, true),
      Threshold(30.gibibytes, PlanAlertType.Autosuspend),
      Threshold(40.gibibytes, PlanAlertType.Autosuspend)
    )
  }

  case object EarlyAlertUsagePlan extends ServicePlan {
    val name = "Early Alert Usage Plan"
    val code = "early_alert_usage"
    val thresholds = Seq(
      Threshold(100.mebibytes),
      Threshold(4.gibibytes),
      Threshold(8.gibibytes),
      Threshold(10.gibibytes, PlanAlertType.Autosuspend, true),
      Threshold(30.gibibytes, PlanAlertType.Autosuspend),
      Threshold(40.gibibytes, PlanAlertType.Autosuspend)
    )
  }

  case object Oc20GbUsagePlan extends ServicePlan {
    val name = "20GB Usage"
    val code = "oc_20gb_usage"
    val thresholds = Seq(
      Threshold(10.gibibytes),
      Threshold(15.gibibytes),
      Threshold(20.gibibytes, PlanAlertType.Autosuspend, true),
      Threshold(40.gibibytes, PlanAlertType.Autosuspend)
    )
  }

  case object OcpExtendedUsagePlan extends ServicePlan {
    import squants.information.InformationConversions._
    val name = "OC Extended Usage"
    val code = "ocp_extended_usage"
    val thresholds = Seq(
      Threshold(4.gibibytes, PlanAlertType.Usage),
      Threshold(8.gibibytes, PlanAlertType.Usage),
      Threshold(10.gibibytes, PlanAlertType.Usage),
      Threshold(20.gibibytes, PlanAlertType.Usage),
      Threshold(40.gibibytes, PlanAlertType.Autosuspend)
    )
  }

  case object OcpTestUsagePlan extends ServicePlan {
    val name = "OC Test Usage"
    val code = "ocp_test_usage"
    val thresholds = Seq(
      Threshold(300.mebibytes),
      Threshold(500.mebibytes),
      Threshold(1.gibibytes, PlanAlertType.Autosuspend),
      Threshold(10.gibibytes, PlanAlertType.Autosuspend, true),
      Threshold(30.gibibytes, PlanAlertType.Autosuspend),
      Threshold(40.gibibytes, PlanAlertType.Autosuspend)
    )
  }

  case object OcpNoAlertPlan extends ServicePlan {
    val name = "OC No-Alert Plan"
    val code = "oc_no_alert_plan"
    val thresholds = Seq()
  }

  val gbToMb = (value:Long) => Gibibytes(value).toMebibytes.toInt

  val kbToBytes = (value:Long) => Kibibytes(value).toBytes

  val mbToKb = (value:Long) => Mebibytes(value).toKibibytes

  val allPlans : Seq[ServicePlan] = Seq(
    OcpStandardUsagePlan,
    Oc20GbUsagePlan,
    OcpExtendedUsagePlan,
    OcpNoAlertPlan,
    EarlyAlertUsagePlan
  )
}

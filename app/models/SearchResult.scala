package models

import play.api.libs.json.{JsValue, Json, Writes}

case class SearchResult[T](currentPage: Int, resultsPerPage: Int, totalResults: Long, items: Seq[T]) {
  val hasOtherResults = totalResults > (resultsPerPage * currentPage)
}

object SearchResult {
  def writes[T](implicit objectWrites:Writes[T]) = new Writes[SearchResult[T]]() {
    override def writes(o: SearchResult[T]): JsValue = {
      implicit val ma = implicitly[Writes[T]]
      Json.obj(
        "currentPage" -> o.currentPage,
        "resultsPerPage" -> o.resultsPerPage,
        "totalResults" -> o.totalResults,
        "hasOtherResults" -> o.hasOtherResults,
        "items" -> o.items.map(i => ma.writes(i))
      )
    }
  }
}

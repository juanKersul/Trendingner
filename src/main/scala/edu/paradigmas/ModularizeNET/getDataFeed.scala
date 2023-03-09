package edu.paradigmas.ModularizeNET

import scalaj.http.{Http, HttpResponse}
import scala.xml.XML

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._


class WebSite(val initialUrl: String, val dataFormat: String = "rss",
  val urlParameter: String = "") {

  private val urlCount = initialUrl.sliding("%s".length).count(slide => slide == "%s")
  val url = urlCount match {
      case 1 => initialUrl.replaceAll("%s", urlParameter)
      case _ => initialUrl
  }

  implicit val formats = DefaultFormats
  val response = getData(url)

  private def getData(_url: String): String =
    try {
      val connTimeoutMs_ = 2000
      val readTimeoutMs_ = 5000

      Http(_url)
        .timeout(connTimeoutMs = connTimeoutMs_, readTimeoutMs = readTimeoutMs_)
        .asString
        .body
    }

    catch {
      case e: Exception => ""
    }

  private def xmlParser(): Seq[String] =
    if (response == "") {
      Seq()
    }

    else {
      // convert the `String` to a `scala.xml.Elem`
      val xml =  XML.loadString(response)
      // Extract text from title and description
      val articlesContent: Seq[String] = (xml \\ "item").map {
        item =>
          (item \ "title").text + " " + (item \ "description").text
      }

      articlesContent
    }

  private def jsonParser(): Seq[String] = {

    if (response=="") {
      Seq()
    }

    else {
      val data = (parse(response) \ "data" \ "children" \ "data").extract[List[Map[String, Any]]]

      val filtredMaps: List[Map[String, Any]] =
        data.filter(
          (listMap:Map[String, Any]) =>
            (listMap.keySet contains "selftext")
            && ((listMap get "selftext").toString.length > 300)
        )

      val filtredRes =
        filtredMaps
          .flatten
          .filter{case (v , _) => v == "title" || v == "selftext" }
          .map(x => x._2.toString)

      val regexUrl = "(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]".r

      filtredRes.map(x => regexUrl.replaceAllIn(x,"")).toSeq
    }
  }

  def parser(): Seq[String] =
    dataFormat match {
      case "rss"  => xmlParser()
      case "reddit" => jsonParser()
      case _      => Seq()
    }
}

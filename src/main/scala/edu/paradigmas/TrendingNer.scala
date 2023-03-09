package edu.paradigmas

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io._


import ModularizeNET.{ WebSite, Model}
import edu.paradigmas.ModularizeNET.subscriptionService

/* Main class */
object TrendingNer extends App {

  val defaultUrl = "https://www.chicagotribune.com/arcio/rss/category/sports/" +
    "?query=display_date:[now-2d+TO+now]&sort=display_date:desc"

  val paragraphs: Seq[String] =
    args.length match {
      case 0 => new WebSite(defaultUrl).parser()
      case 1 => new WebSite(args(0)).parser()
      case _ =>
        if (args(0) != "path") {
          val url = args(0)
          val format = args(1)

          new  WebSite(url, format).parser()
        }

        else {
          val path = args(1)
          new subscriptionService(path).getFeed()
        }
  }

  // *********************** APPLY THE MODEL ***************************
  val model = new Model
  val words: Seq[String] = model.extractedNEs(paragraphs)

  // ****************** COUNT AND SORT THE ENTITIES ************************

  val countedWords: Map[String, Int] = model.neCounts(words)
  val sortedResults: List[(String, Int)] = model.sortedNEs(countedWords)

  // ****************** PRINT RESULTS ************************

  sortedResults.foreach {
    case (str, n) =>
      println(s"$str : $n")
  }
}

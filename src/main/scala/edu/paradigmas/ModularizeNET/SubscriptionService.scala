package edu.paradigmas.ModularizeNET

import scala.io.Source
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

class subscriptionService(path: String = "subscriptions.josn") {
    implicit val formats = DefaultFormats

    def getData(): List[Map[String, Any]] = {
        val file = Source.fromFile(path).mkString
        val data = parse(file).extract[List[Map[String, Any]]]
        data
    }

    def getFeed(): Seq[String] = {
        val data:List[Map[String, Any]] = getData()

        val res = data.foldLeft[Seq[String]](Seq.empty[String]) (
            (seq: Seq[String], dict: Map[String,Any])  =>
                seq ++ dictFunction(dict)
        )

        res
    }

    def dictFunction(dict:Map[String,Any]): Seq[String] = {
        val url = (dict get "url").mkString
        val urlType = (dict get "urlType").mkString

        val list_of_params: List[String] = (dict get "urlParams").head.asInstanceOf[List[String]]

        val res =

            if(list_of_params.nonEmpty) {
                list_of_params.foldLeft[Seq[String]](Seq.empty[String]) (
                    (seq: Seq[String], param: Any) =>
                        seq ++ (new WebSite(url, urlType, param.toString())).parser()
                )
            }

            else {
                (new WebSite(url, urlType)).parser()
            }

        res
    }
}

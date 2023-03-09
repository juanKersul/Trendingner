package edu.paradigmas.ModularizeNET

//case class NERCount(ner: String, count:Int)

class Model {

  val stopwords = Seq (
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you",
    "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she",
    "her", "hers", "herself", "it", "its", "itself", "they", "them", "your",
    "their", "theirs", "themselves", "what", "which", "who", "whom",
    "this", "that", "these", "those", "am", "is", "are", "was", "were",
    "be", "been", "being", "have", "has", "had", "having", "do", "does",
    "did", "doing", "a", "an", "the", "and", "but", "if", "or",
    "because", "as", "until", "while", "of", "at", "by", "for", "with",
    "about", "against", "between", "into", "through", "during", "before",
    "after", "above", "below", "to", "from", "up", "down", "in", "out",
    "off", "over", "under", "again", "further", "then", "once", "here",
    "there", "when", "where", "why", "how", "all", "any", "both", "each",
    "few", "more", "most", "other", "some", "such", "no", "nor", "not",
    "only", "own", "same", "so", "than", "too", "very", "s", "t", "can",
    "will", "just", "don", "should", "now", "on",
    // Contractions without '
    "im", "ive", "id", "Youre", "youd", "youve",
    "hes", "hed", "shes", "shed", "itd", "were", "wed", "weve",
    "theyre", "theyd", "theyve",
    "shouldnt", "couldnt", "musnt", "cant", "wont",
    // Common uppercase words
    "hi", "hello"
  )

  def getNEsSingle(text: String): Seq[String] = {
    val punctuationSymbols = ".,()!?;:'`’´\n"
    val punctuationRegex = "\\" + punctuationSymbols.split("").mkString("|\\")

    val textWhithOutSymbols = text.replaceAll(punctuationRegex, "")
    val arrayOfWords = textWhithOutSymbols.split(" ")
    val filteredArray = arrayOfWords.filter {
      word:String =>
        word.length > 1 &&                        // tiene más de una letra
        Character.isUpperCase(word.charAt(0)) &&  // empieza con mayuscula
        !stopwords.contains(word.toLowerCase)     // no aparece en STOPWORDS
    }

    filteredArray.toSeq
  }

  /* Limpia el texto y lo separa por " " */
  def extractedNEs(parsedData: Seq[String]): Seq[String] = {
    parsedData.map(getNEsSingle).flatten
  }

  /* Método que cuenta las entidades que separamos anteriormente.*/
  def neCounts(extractedNes: Seq[String]): Map[String, Int] =
    extractedNes.foldLeft(Map.empty[String, Int]) {
      (map, str) => map + (str -> (map.getOrElse(str, 0) + 1))
    }

  /* Toma las entidades contadas y las ordena. */
  def sortedNEs(neCounted: Map[String, Int]): List[(String, Int)] =
    neCounted.toList.sortBy(_._2)(Ordering[Int].reverse)

}

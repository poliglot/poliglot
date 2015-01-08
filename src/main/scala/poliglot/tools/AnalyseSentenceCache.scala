package poliglot.tools

import poliglot.formats.SentenceCache

import scala.collection.mutable
import scala.util.Try

object AnalyseSentenceCache {
  def run(inputPath: String) {
    val trainingData =
      Try(SentenceCache.read(inputPath))
        .getOrElse(SentenceCache(mutable.ArrayBuffer.empty))

    val bad = trainingData.translations.filter(!_.good)
    val good = trainingData.translations.filter(_.good)
    val total = trainingData.translations.size
    val badRatio = bad.size.toDouble / total
    val goodRatio = good.size.toDouble / total

    println(s"Bad: ${bad.size} ($badRatio)")
    println(s"Good: ${good.size} ($goodRatio)")
    println(s"Total: $total")
  }

  def main(args: Array[String]) {
    run("../parallel-de-pl/sentences.xml")
  }
}

package poliglot.tools

import poliglot.Input
import poliglot.formats.{Translation, SentenceCache, TMX}
import poliglot.languages.German

import scala.collection.mutable
import scala.util.Try

object SelectSentences {
  val MaxSentencesPerPreposition = 20

  /* Filter out sentences that are too long to save some time during
   * manual annotation.
   */
  val MaxTokens = 12

  /* Take every n-th sentence, to prevent translator bias. Closely appearing
   * sentences in the TMX file are likely to originate from the same source.
   */
  val DeltaIndex = 150

  def ask(): Boolean = {
    print("Good translation? [y/n] ")
    val ipt = Console.in.read()
    if (ipt == 'y') true
    else if (ipt == 'n') false
    else ask()
  }

  def process(trainingData: SentenceCache, source: String, target: String): Boolean = {
    val total = trainingData.translations.size
    val good = trainingData.translations.count(_.good)
    val bad = trainingData.translations.count(!_.good)

    println(s"Translations: $total ($good good, $bad bad)")
    println(source)
    println(target)
    println()

    val isGood = ask()
    trainingData.translations += Translation(source, target, isGood)
    isGood
  }

  def run(tmxPath: String, xmlPath: String) {
    println("Reading...")

    val queueDePrepositions = mutable.HashSet(German.Adpositions.toSeq: _*)
    val counts = mutable.HashMap.empty[String, Int].withDefaultValue(0)

    def prepIter(intersection: Set[String]) {
      intersection.foreach(prep => counts(prep) += 1)
      counts.foreach { case (prep, cnt) =>
        if (cnt >= MaxSentencesPerPreposition)
          queueDePrepositions.remove(prep)
      }
    }

    val trainingData =
      Try(SentenceCache.read(xmlPath))
        .getOrElse(SentenceCache(mutable.ArrayBuffer.empty))

    println("Read sentence cache")

    trainingData.translations.foreach { tr =>
      val words = Input.tokens(tr.source).map(_.toLowerCase).toSet

      if (tr.good) {
        val intersection = words & queueDePrepositions
        prepIter(intersection)
      }
    }

    var count = 0
    TMX.readFile(tmxPath) { case (source, target) =>
      if (count % DeltaIndex == 0) {
        val words = Input.tokens(source).map(_.toLowerCase).toSet
        val intersection = words & queueDePrepositions

        if (intersection.isEmpty)
          println("Does not contain any prepositions; skipping")
        else if (words.size > MaxTokens)
          println("Too many tokens; skipping")
        else {
          println(s"Preposition counts: " + counts.mkString(", "))
          val m = trainingData.translations.find(_.source == source)

          if (!m.isDefined) {
            val good = process(trainingData, source, target)
            SentenceCache.write(xmlPath, trainingData)
            if (good) {
              println("Inserted alignment")
              prepIter(intersection)
            }
          }
        }
      }

      count += 1
      queueDePrepositions.nonEmpty
    }

    println("Done")
  }

  def main(args: Array[String]) {
    run(
      "../parallel-de-pl/de-pl.tmx",
      "../parallel-de-pl/sentences.xml"
    )
  }
}

package poliglot.tools

import java.io.FileWriter

import poliglot.formats
import poliglot.formats._
import poliglot.tagging.Concraft
import poliglot.utils.FileHelpers

import scala.collection.mutable
import scala.util.Try

object AnnotateSentences {
  def identifyGermanTokens(concraft: String, translations: Seq[Translation], outPath: String) {
    val tokens = new FileWriter(outPath)

    translations.foreach { tr =>
      val tokensSource = Concraft.tokens(Concraft.lookUp(concraft, tr.source))
      tokensSource.foreach { tk => tokens.write(tk.orth + "\n") }
      tokens.write("\n")
    }

    tokens.close()
  }

  def fixCharset(str: String) = {
    /* Some German lemmas with do not encode special characters correctly. This
     * seems to be a bug in RFTagger.
     */
    str.replaceAll("Ã\u009F", "ß")
      .replaceAll("Ã¶", "ö")
      .replaceAll("Ã¼", "ü")
  }

  def tagGermanTokens(taggerPath: String, tokensPath: String): List[List[AnnotatedToken]] = {
    import scala.sys.process._

    val sentences = Seq(taggerPath, tokensPath)
      .lineStream_!
      .toList
      .mkString("\n")

    sentences.split("\n\n").map { translation =>
      translation.split("\n").map { line =>
        val parts = line.split("\t")

        val orth = parts(0)
        val lemma = if (parts(2) == "<unknown>") orth else fixCharset(parts(2))
        val tag = parts(1).split('.')

        AnnotatedToken(orth, lemma, tag)
      }.toList
    }.toList
  }

  def run(concraft: String,
          taggerPath: String,
          inputPath: String,
          outputPath: String)
  {
    val trainingData =
      Try(SentenceCache.read(inputPath))
        .getOrElse(formats.SentenceCache(mutable.ArrayBuffer.empty))

    val good = trainingData.translations.filter(_.good)
    println(s"Read ${good.size} sentences from cache")

    val tmpTokensPath = "/tmp/sentences-de.txt"
    identifyGermanTokens(concraft, good, tmpTokensPath)
    println("German tokens identified")

    val taggedDe = tagGermanTokens(taggerPath, tmpTokensPath)
    println("German tokens tagged")

    val taggedPl = good.map { tr =>
      Concraft.tokens(Concraft.lookUp(concraft, tr.target))
    }

    assert(taggedDe.size == taggedPl.size)

    val corpus = AnnotatedSentences.Corpus(
      taggedDe.zip(taggedPl).map { case (de, pl) =>
        AnnotatedSentences.Translation(de, pl) }
    )

    println("Polish tagged")

    FileHelpers.write(outputPath, corpus.toXml)

    println(s"Annotated corpus written to $outputPath")
  }

  def main(args: Array[String]) {
    run(
      "192.168.2.176",
      "../language-de/scripts/rftagger",
      "../parallel-de-pl/sentences.xml",
      "../parallel-de-pl/alignments-import.xml")
  }
}

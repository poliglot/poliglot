package poliglot.formats

import java.io.FileWriter

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._

object Tiger {
  case class Terminal(word: String, lemma: String, features: List[String])

  def attrValue(md: MetaData, key: String): String = {
    val value = md(key).head.text.toLowerCase

    if (value == "--") ""
    else if (value == "*") ""
    else value
  }

  def formatLemma(md: MetaData): String = {
    val value = md("lemma").head.text
    if (value == "--") md("word").head.text
    else value
  }

  def formatPos(md: MetaData): String = {
    val value = attrValue(md, "pos")

    if (value == "$(") "intpint" // Sonstige Satzzeichen; satzintern
    else if (value == "$,") "intpcom" // Komma
    else if (value == "$.") "intpdot" // Satzbeendende Interpunktion
    else value
  }

  def processTerminal(md: MetaData): Terminal = {
    val word = md("word").head.text
    val lemma = formatLemma(md)

    val pos = formatPos(md)
    val cas = attrValue(md, "case")
    val nmb = attrValue(md, "number")
    val gnd = attrValue(md, "gender")
    val per = attrValue(md, "person")
    val deg = attrValue(md, "degree")
    val ten = attrValue(md, "tense")
    val mod = attrValue(md, "mood")

    /* The Tiger corpus underspecifies some attributes. */
    val features = List(pos, nmb, cas, gnd, per, ten, deg, mod).filter(_.nonEmpty)
    Terminal(word, lemma, features)
  }

  def readFile(path: String, attr: MetaData => Unit, next: () => Boolean) {
    val xml = new XMLEventReader(
      Source.fromFile(path, "ISO-8859-1"))
    var insideTerminals = false

    for (event <- xml) {
      event match {
        case EvElemStart(_, "t", attrs, _) if insideTerminals =>
          attr(attrs)
        case EvElemStart(_, "terminals", _, _) => insideTerminals = true
        case EvElemEnd(_, "terminals") =>
          insideTerminals = false
          if (!next()) return
        case _ =>
      }
    }
  }

  /**
   * Converts the Tiger dump to the ``plain text format`` as specified here:
   * http://nlp.pwr.wroc.pl/redmine/projects/corpus2/wiki/Inputoutput_formats (last example)
   */
  def convert(inputPath: String, outputPath: String) {
    val out = new FileWriter(outputPath)

    var count = 0

    readFile(inputPath, attrs => {
      val term = processTerminal(attrs)
      val features = term.features.mkString(":")
      out.write(s"${term.word}\tspace\n")
      out.write(s"\t${term.lemma}\t$features\tdisamb\n")
    }, () => {
      out.write("\n")
      if (count % 100 == 0) println("count = " + count)
      count += 1
      true
    })

    out.close()
  }
}
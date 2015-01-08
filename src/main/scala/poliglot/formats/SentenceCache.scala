package poliglot.formats

import java.io.FileWriter
import scala.collection.mutable
import scala.xml.{NodeSeq, XML}

/** The local corpus capturing seen sentences and a flag. */

case class Translation(source: String, target: String, good: Boolean) {
  def toXml =
    <translation>
      <source>{source}</source>
      <target>{target}</target>
      <good>{good}</good>
    </translation>
}

case class SentenceCache(translations: mutable.ArrayBuffer[Translation]) {
  def toXml =
    <translations>
      {translations.map(_.toXml)}
    </translations>
}

/**
 * Local file format for translation training data. The file contains
 * information regarding entities and their alignment. For now restricted
 * only to the top-level.
 */
object SentenceCache {
  def read(path: String): SentenceCache = {
    val xml = XML.loadFile(path)

    def parseTranslation(node: NodeSeq) =
      Translation(
        source = (node \\ "source").head.text.trim,
        target = (node \\ "target").head.text.trim,
        good = (node \\ "good").headOption.map(_.text.trim == "true").getOrElse((node \\ "alignment").nonEmpty))

    SentenceCache(mutable.ArrayBuffer((xml \\ "translation").map(parseTranslation): _*))
  }

  def write(path: String, data: SentenceCache) {
    val printer = new scala.xml.PrettyPrinter(80, 2)
    val file = new FileWriter(path)
    file.write(printer.format(data.toXml))
    file.close()
  }
}
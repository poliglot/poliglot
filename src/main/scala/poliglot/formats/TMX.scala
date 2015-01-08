package poliglot.formats

import scala.collection.mutable
import scala.io.Source
import scala.xml.pull._

object TMX {
  def readFile(path: String)(f: (String, String) ⇒ Boolean) {
    val xml = new XMLEventReader(Source.fromFile(path))
    val buffer = mutable.ArrayBuffer.empty[String]
    var insideSeg = false

    for (event <- xml) {
      event match {
        case EvElemEnd(_, "tu") =>
          if (buffer.size == 2 && !f(buffer(0), buffer(1))) return
          buffer.clear()
        case EvElemStart(_, "seg", _, _) => insideSeg = true
        case EvText(t) if insideSeg => buffer += t
        case EvElemEnd(_, "seg") =>
          insideSeg = false
        case _ =>
      }
    }
  }
}

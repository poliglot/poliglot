package poliglot.utils

import java.io.File

import scala.xml.Elem

object FileHelpers {
  def write(path: String, contents: String) {
    val pw = new java.io.PrintWriter(new File(path))
    try pw.write(contents) finally pw.close()
  }

  def write(path: String, xml: Elem) {
    val printer = new scala.xml.PrettyPrinter(80, 2)
    FileHelpers.write(path, printer.format(xml))
  }

  def basename(path: String) =
    path.split('/').last.split('.').head
}

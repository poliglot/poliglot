package poliglot.formats

import poliglot.Input

object Dictcc {
  def clean(value: String) =
    value
      .replaceAll("\\{.*?\\}", "")
      .replaceAll("\\[.*?\\]", "")
      .replaceAll("\\<.*?\\>", "")
      .filterNot(Input.punctuation.contains)
      .replaceAll("  ", " ")
      .trim()

  def read(path: String): Set[(String, String)] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines()

    val res = lines
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map
    { line =>
      val parts = line.split('\t')
      (clean(parts(0)), clean(parts(1)))
    }.toSet

    source.close()
    res
  }
}

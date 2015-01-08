package poliglot.formats

object TabDictionary {
  def read(path: String): Set[(String, String)] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines()

    val res = lines
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map
      { line =>
        val parts = line.split('\t')
        (parts(0).trim, parts(1).trim)
      }.toSet

    source.close()
    res
  }
}

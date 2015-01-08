package poliglot.formats

import scala.xml.{Node, NodeSeq, XML}

/**
 * Parser for CCL files
 */
object CCL {
  def parse(path: String): Seq[Seq[AnnotatedToken]] = {
    val xml = XML.loadFile(path)

    def parseToken(node: Node) =
      AnnotatedToken(
        orth = (node \\ "orth").head.text,
        lemma = (node \\ "base").head.text,
        tagParts = (node \\ "ctag").head.text.split(':')
      )

    def parseSentence(nodes: NodeSeq) =
      (nodes \\ "tok") .map(parseToken)

    (xml \\ "sentence").map(parseSentence)
  }
}

package poliglot.formats

/**
 * Morphosyntactically annotated token
 */
case class AnnotatedToken(orth: String, lemma: String, tagParts: Array[String]) {
  override def toString = orth

  def toXml =
    <token>
      <orth>{orth}</orth>
      <lemma>{lemma}</lemma>
      <tag>{tagParts.mkString(":")}</tag>
    </token>
}

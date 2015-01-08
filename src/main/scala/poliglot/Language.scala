package poliglot

trait Language {
  val Adpositions: Set[String]
  def partOfSpeech(tagParts: Seq[String]): POS
  def morphAttributes(lemma: String, tagParts: Seq[String]): Set[MorphAttribute]
}

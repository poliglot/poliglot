package poliglot.formats

/**
 * Corpus of morphosyntactically annotated sentences
 */
object AnnotatedSentences {
  case class Corpus(translations: Seq[Translation]) {
    def toXml = <translations>{translations.map(_.toXml)}</translations>
  }
  
  case class Translation(source: Seq[AnnotatedToken], target: Seq[AnnotatedToken]) {
    def toXml =
      <translation>
        <source><tokens>{source.map(_.toXml)}</tokens></source>
        <target><tokens>{target.map(_.toXml)}</tokens></target>
      </translation>
  }
}

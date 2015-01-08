package poliglot

/* Represents normalised POS tag of a token across all languages */
trait POS
object POS {
  case object Article extends POS
  case object Pronoun extends POS
  case object Verb extends POS
  case object Adverb extends POS
  case object Noun extends POS
  case object Adjective extends POS
  case object Conjunction extends POS
  case object Punctuation extends POS
  case object Particle extends POS
  case object Adposition extends POS
  case object Clitic extends POS
  case object Number extends POS // TODO How is this different from Cardinal?
  case object Interjection extends POS
  case object Abbreviation extends POS
  case object Expression extends POS
  case object Foreign extends POS
  case object Other extends POS
}

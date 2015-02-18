package poliglot

/**
 * Common morphosyntactical attributes represented as a class hierarchy
 *
 * Before adding new attributes, please evaluate the genericity across
 * languages and mind the proper linguistic naming.
 */
trait MorphAttribute
object MorphAttribute {
  trait Pronoun extends MorphAttribute
  object Pronoun {
    case object Personal extends Pronoun
    case object Possessive extends Pronoun
  }

  /* Punctuation */
  case object DotInternal extends MorphAttribute
  case object DotSentence extends MorphAttribute

  case object Name extends MorphAttribute
  case object Impersonal extends MorphAttribute

  trait Verb extends MorphAttribute
  object Verb {
    case object Predicative extends Verb
    case object Participle extends Verb
    case object Auxiliary extends Verb
    case object Modal extends Verb
    case object Full extends Verb
    case object Infinitive extends Verb
    case object Finite extends Verb
    case object Gerund extends Verb
  }

  trait Conjunction extends MorphAttribute
  object Conjunction {
    // for, and, nor, but, or, yet, so
    case object Coordinating extends Conjunction

    // pl. spójniki podrzędne
    // aby, bo, choć, czy, jeżeli, ponieważ, że, bowiem
    case object Subordinate extends Conjunction
  }

  trait Compounding extends MorphAttribute
  object Compounding {
    /** German: zum (zu + dem) */
    case object Compound extends Compounding
  }

  trait Adposition extends MorphAttribute
  object Adposition {
    case object Preposition extends Adposition
    case object Postposition extends Adposition
    case object Circumposition extends Adposition
  }

  trait Adjective extends MorphAttribute
  object Adjective {
    /* Polish: polsko-niemiecki (przymiotnik przyprzymiotnikowy)
     * TODO Find better translation
     */
    case object Adadjectival extends Adjective

    /* Polish: po polsku (post-prepositional adjective) */
    case object Prepositional extends Adjective

    case object Predicative extends Adjective
    case object Attributive extends Adjective
  }

  trait Degree extends MorphAttribute
  object Degree {
    case object Positive extends Degree
    case object Comparative extends Degree
    case object Superlative extends Degree
  }

  /* Numeral agreement */
  trait Agreement extends MorphAttribute
  object Agreement {
    /* Polish: Inherit case from noun; dwie rzeczy */
    case object Congruent extends Agreement

    /* Polish: Require the noun to use the genitive case */
    case object Governing extends Agreement
  }

  trait Epenthesis extends MorphAttribute
  object Epenthesis {
    /* Insertion of a vowel
     * Polish: ze
     */
    case object Anaptyxis extends Epenthesis

    /* Polish: z */
    case object None extends Epenthesis
  }

  trait Contraction extends MorphAttribute
  object Contraction {
    /* Polish: jego, niego, jemu
     * French: tu, le/la
     */
    case object None extends Contraction

    /* French: t', l' etc. (elision)
     * Polish: go, -ń, mu
     */
    case object Contracted extends Contraction
  }

  trait Clitic extends MorphAttribute
  object Clitic {
    /* French: t', s'
     */
    case object Proclitic extends Clitic

    /* Auxiliary clitic
     * Polish: -(e)m, -(e)ście, -ń (as in dlań - dla niego)
     */
    case object Enclitic extends Clitic

    /* Auxiliary clitic
     * Polish: -(e)m, -(e)ście, -ń (as in dlań - dla niego)
     */
    case object Agglutinant extends Clitic

    /* Reflexive marker
     * Polish: się
     * See also http://link.springer.com/article/10.1007%2Fs11185-010-9062-7 (p. 232)
     */
    case object Refl extends Clitic
  }

  trait Participle extends MorphAttribute
  object Participle {
    // pl. imiesłów przymiotnikowy czynny
    case object ActiveAdjectival extends Participle

    // pl. imiesłów przymiotnikowy bierny
    case object PassiveAdjectival extends Participle

    // pl. imiesłów przysłówkowy współczesny
    case object PresentAdverbial extends Participle

    // pl. imiesłów przysłówkowy uprzedni
    case object PerfectAdverbial extends Participle
  }

  trait Mood extends MorphAttribute
  object Mood {
    case object Indicative extends Mood
    case object Subjunctive extends Mood
    case object Conditional extends Mood
    case object Imperative extends Mood
  }

  trait Definiteness extends MorphAttribute
  object Definiteness {
    case object Definite extends Definiteness
    case object Indefinite extends Definiteness
  }

  trait Polarity extends MorphAttribute
  object Polarity {
    case object Affirmative extends Polarity
    case object Negative extends Polarity
  }

  trait Person extends MorphAttribute
  object Person {
    case object First extends Person
    case object Second extends Person
    case object Third extends Person
  }

  trait Aspect extends MorphAttribute
  object Aspect {
    case object Perfect extends Aspect
    case object Imperfect extends Aspect
  }

  trait Number extends MorphAttribute
  object Number {
    case object Singular extends Number
    case object Plural extends Number
  }

  trait Gender extends MorphAttribute
  object Gender {
    case object Masculine extends Gender
    case object Feminine extends Gender
    case object Neuter extends Gender
  }

  trait Tense extends MorphAttribute
  object Tense {
    case object Imperfect extends Tense
  }

  trait Case extends MorphAttribute
  object Case {
    case object Accusative extends Case
    case object Dative extends Case
    case object Genitive extends Case
    case object Nominative extends Case
    case object Locative extends Case
    case object Instrumental extends Case
    case object Vocative extends Case
  }
}

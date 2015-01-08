package poliglot.languages

import poliglot.MorphAttribute._
import poliglot.{MorphAttribute, Language, POS}

/**
 * Parser for the NKJP tagset as it is used by concraft-pl.
 *
 * See also Adam Przepiórkowski. Narodowy Korpus Języka Polskiego (2012), pp. 72f, 77f
 */
object Polish extends Language {
  val Adpositions = Set("z", "do")

  def partOfSpeech(tagParts: Seq[String]): POS =
    tagParts.head match {
      case "adj" => POS.Adjective
      case "adja" => POS.Adjective
      case "adjp" => POS.Adjective
      case "adjc" => POS.Adjective

      case "adv" => POS.Adverb

      case "prep" => POS.Adposition

      case "ppron12" => POS.Pronoun
      case "ppron3" => POS.Pronoun
      case "siebie" => POS.Pronoun

      case "num" => POS.Number
      case "numcol" => POS.Number // TODO How to differentiate from ``num``?

      /* Grouped together as suggested in the NKJP book (p. 116) */
      case "subst" => POS.Noun
      case "ger" => POS.Noun
      case "depr" => POS.Noun // TODO Define Noun.Depreciate?

      case "conj" => POS.Conjunction
      case "comp" => POS.Conjunction

      // TODO More specific morphological attributes should be assigned here, too.
      case "inf" => POS.Verb
      case "fin" => POS.Verb
      case "bedzie" => POS.Verb
      case "winien" => POS.Verb
      case "praet" => POS.Verb
      case "impt" => POS.Verb
      case "pcon" => POS.Verb
      case "pant" => POS.Verb
      case "pact" => POS.Verb
      case "ppas" => POS.Verb
      case "pred" => POS.Verb
      case "imps" => POS.Verb

      case "interj" => POS.Interjection
      case "interp" => POS.Punctuation
      case "qub" => POS.Clitic
      case "aglt" => POS.Clitic

      case "brev" => POS.Abbreviation
      case "xxx" => POS.Foreign
      case "burk" => POS.Expression
    }

  def morphAttributes(orth: String, tagParts: Seq[String]): Set[MorphAttribute] = {
    tagParts.flatMap {
      case "sg" => Set[MorphAttribute](Number.Singular)
      case "pl" => Set[MorphAttribute](Number.Plural)

      case "nom" => Set[MorphAttribute](Case.Nominative)
      case "gen" => Set[MorphAttribute](Case.Genitive)
      case "dat" => Set[MorphAttribute](Case.Dative)
      case "acc" => Set[MorphAttribute](Case.Accusative)
      case "inst" => Set[MorphAttribute](Case.Instrumental)
      case "loc" => Set[MorphAttribute](Case.Locative)
      case "voc" => Set[MorphAttribute](Case.Vocative)

      case "m1" => Set[MorphAttribute](Gender.Masculine, Person.First)
      case "m2" => Set[MorphAttribute](Gender.Masculine, Person.Second)
      case "m3" => Set[MorphAttribute](Gender.Masculine, Person.Third)
      case "f" => Set[MorphAttribute](Gender.Feminine)
      case "n" => Set[MorphAttribute](Gender.Neuter)

      case "pri" => Set[MorphAttribute](Person.First)
      case "sec" => Set[MorphAttribute](Person.Second)
      case "ter" => Set[MorphAttribute](Person.Third)

      case "pos" => Set[MorphAttribute](Degree.Positive)
      case "com" => Set[MorphAttribute](Degree.Comparative)
      case "sup" => Set[MorphAttribute](Degree.Superlative)

      case "imperf" => Set[MorphAttribute](Aspect.Imperfect)
      case "perf" => Set[MorphAttribute](Aspect.Perfect)

      case "aff" => Set[MorphAttribute](Polarity.Affirmative)
      case "neg" => Set[MorphAttribute](Polarity.Negative)

      case "congr" => Set[MorphAttribute](Agreement.Congruent)
      case "rec" => Set[MorphAttribute](Agreement.Governing)

      case "praep" if orth == "ń" => Set[MorphAttribute](Clitic.Agglutinant) // -ń
      case "praep" => Set[MorphAttribute](Clitic.Enclitic) // niego
      case "npraep" => Set[MorphAttribute](Clitic.Proclitic) // jego, go

      case "akc" => Set[MorphAttribute](Contraction.None) // jego, niego, jemu
      case "nakc" => Set[MorphAttribute](Contraction.Contracted) // go, -ń, mu

      /* Appears scarcely. If need arises, an attribute could be created. */
      case "agl" => Set[MorphAttribute]()
      case "nagl" => Set[MorphAttribute]()

      case "wok" => Set[MorphAttribute](Epenthesis.Anaptyxis)
      case "nwok" => Set[MorphAttribute](Epenthesis.None)

      /* POS refinements */
      case "qub" if orth == "się" => Set[MorphAttribute](Clitic.Refl)
      case "qub" if orth == "by" => Set[MorphAttribute](Mood.Conditional)
      case "qub" if orth == "nie" => Set[MorphAttribute](Polarity.Negative)
      case "conj" => Set[MorphAttribute](Conjunction.Coordinating)
      case "comp" => Set[MorphAttribute](Conjunction.Subordinate)
      case "aglt" => Set[MorphAttribute](Clitic.Agglutinant) // -ście, -ś, -m
      case "prep" => Set[MorphAttribute](Adposition.Preposition)
      case "praet" => Set[MorphAttribute](Tense.Imperfect)
      case "impt" => Set[MorphAttribute](Mood.Imperative)
      case "adja" => Set[MorphAttribute](Adjective.Adadjectival)
      case "adjp" => Set[MorphAttribute](Adjective.Prepositional)
      case "adjc" => Set[MorphAttribute](Adjective.Predicative)
      case "pcon" => Set[MorphAttribute](Participle.PresentAdverbial)
      case "pant" => Set[MorphAttribute](Participle.PerfectAdverbial)
      case "pact" => Set[MorphAttribute](Participle.ActiveAdjectival)
      case "ppas" => Set[MorphAttribute](Participle.PassiveAdjectival)
      case "inf" => Set[MorphAttribute](Verb.Infinitive)
      case "fin" => Set[MorphAttribute](Verb.Finite)
      case "ger" => Set[MorphAttribute](Verb.Gerund)
      case "imps" => Set[MorphAttribute](Impersonal)

      case _ => Set[MorphAttribute]()
    }.toSet
  }
}

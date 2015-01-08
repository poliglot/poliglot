package poliglot.languages

import poliglot.MorphAttribute._
import poliglot.{MorphAttribute, Language, POS}

/**
 * This file parses the tagset used by RFTagger.
 */
object German extends Language {
  /* Subset of adpositions that were analysed in tools/. */
  val Adpositions = Set("an", "auf", "in", "mit", "nach", "um", "zu", "über")

  /** TODO Should return Set[POS]. */
  def partOfSpeech(tagParts: Seq[String]): POS =
    tagParts.headOption match {
      case Some("PRO") => POS.Pronoun
      case Some("PROADV") => POS.Pronoun // TODO Set(POS.Pronoun, POS.Adverb)
      case Some("APPR") => POS.Adposition
      case Some("APPO") => POS.Adposition
      case Some("APZR") => POS.Adposition
      case Some("APPRART") => POS.Adposition // TODO Set(POS.Adposition, POS.Article)
      case Some("VFIN") => POS.Verb
      case Some("VINF") => POS.Verb
      case Some("VIMP") => POS.Verb
      case Some("VPP") => POS.Verb
      case Some("N") => POS.Noun
      case Some("ART") => POS.Article
      case Some("ADV") => POS.Adverb
      case Some("ADJA") => POS.Adjective
      case Some("ADJD") => POS.Adjective
      case Some("CONJ") => POS.Conjunction
      case Some("SYM") => POS.Punctuation
      case Some("PART") if tagParts(1) == "Verb" => POS.Verb // TODO Set(POS.Particle, POS.Verb)
      case Some("PART") => POS.Particle
      case Some("CARD") => POS.Number
      case Some("ITJ") => POS.Interjection
      case Some("FM") => POS.Foreign
    }

  def morphAttributes(orth: String, tagParts: Seq[String]): Set[MorphAttribute] = {
    tagParts.flatMap {
      case "APPO" => Set[MorphAttribute](Adposition.Postposition)
      case "APPR" => Set[MorphAttribute]() // TODO May also be a Adposition.Circumposition if later a APZR appeares
      case "APZR" => Set[MorphAttribute](Adposition.Circumposition)
      case "APPRART" => Set[MorphAttribute](Adposition.Preposition, Compounding.Compound)
      case "Full" => Set[MorphAttribute](Verb.Full)
      case "Mod" => Set[MorphAttribute](Verb.Modal)

      case "Ind" => Set[MorphAttribute](Mood.Indicative)
      case "VIMP" => Set[MorphAttribute](Mood.Imperative)

      case "VINF" => Set[MorphAttribute](Verb.Infinitive)
      case "VPP" => Set[MorphAttribute](Verb.Participle)
      case "Aux" => Set[MorphAttribute](Verb.Auxiliary)

      case "1" => Set[MorphAttribute](Person.First)
      case "2" => Set[MorphAttribute](Person.Second)
      case "3" => Set[MorphAttribute](Person.Third)

      case "Pos" => Set[MorphAttribute](Degree.Positive)
      case "Comp" => Set[MorphAttribute](Degree.Comparative)
      case "Sup" => Set[MorphAttribute](Degree.Superlative)

      case "Dem" => Set[MorphAttribute]()  // TODO Demonstrative
      case "Intr" => Set[MorphAttribute]() // TODO Interrogative

      case "Pers" => Set[MorphAttribute](Pronoun.Personal)
      case "Poss" => Set[MorphAttribute](Pronoun.Possessive)
      case "Refl" => Set[MorphAttribute]()  // TODO Reflexive
      case "Rel" => Set[MorphAttribute]()   // TODO Relative
      case "Subst" => Set[MorphAttribute]() // TODO Substituting
      case "Attr" => Set[MorphAttribute]()  // TODO Attributive

      case "Def" => Set[MorphAttribute](Definiteness.Definite)
      case "Indef" => Set[MorphAttribute](Definiteness.Indefinite)

      case "Acc" => Set[MorphAttribute](Case.Accusative)
      case "Gen" => Set[MorphAttribute](Case.Genitive)
      case "Dat" => Set[MorphAttribute](Case.Dative)
      case "Nom" => Set[MorphAttribute](Case.Nominative)
      case "Neut" => Set[MorphAttribute](Gender.Neuter)
      case "Sg" => Set[MorphAttribute](Number.Singular)
      case "Pl" => Set[MorphAttribute](Number.Plural)
      case "Masc" => Set[MorphAttribute](Gender.Masculine)
      case "Fem" => Set[MorphAttribute](Gender.Feminine)
      case "Punct" => Set[MorphAttribute](DotSentence)
      case "Neg" => Set[MorphAttribute](Polarity.Negative)
      case "Name" => Set[MorphAttribute](Name)
      case _ => Set[MorphAttribute]()
    }.toSet
  }
}
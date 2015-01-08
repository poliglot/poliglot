package poliglot.tools

import poliglot.formats.{Lemma, Lemmas, TrainingCorpus}

object ExtractLemmas {
  def numTokens(tks: (Int, Int)) =
    tks match {
      case (x, y) => y - x
    }
  
  def run(sentencesPath: String, lemmasPath: String) {
    val corpus = TrainingCorpus.read(sentencesPath)
    val lemmas = Lemmas.read(lemmasPath)
    
    println(s"Read ${lemmas.size} lemmas")
    
    corpus.translations.foreach { translation =>
      translation.source.entities
        .filter(entity =>
          entity.alignment.isDefined &&
          numTokens(entity.tokens) == 1
        )
        .foreach { srcEntity =>
          val tgtEntity = translation.target.entities
            .find(_.id == srcEntity.alignment.get).get

          val srcTokens = (srcEntity.tokens._1 until srcEntity.tokens._2)
            .map(translation.source.tokens)
          val tgtTokens = (tgtEntity.tokens._1 until tgtEntity.tokens._2)
            .map(translation.target.tokens)

          val potentialLemmas =
            srcTokens.zip(tgtTokens)
              .map { case (l, r) => Lemma(l.lemma, r.lemma) }
              .filter { case Lemma(l, r) => l != r }

          potentialLemmas.foreach { lemma =>
            if (!lemmas.contains(lemma)) println(lemma)
          }
        }
    }
  }

  def main(args: Array[String]) {
    run(
      "../parallel-de-pl/alignments.xml",
      "../parallel-de-pl/lemmas.txt"
    )
  }
}

package poliglot.tools

import poliglot.formats.TrainingCorpus

import scala.collection.mutable

object AnalyseCorpus {

  def calculateLevel(ents: Seq[TrainingCorpus.Entity], ent: TrainingCorpus.Entity): Int = {
    if (ent.parent == -1) 0
    else {
      val parent = ents.find(_.id == ent.parent)
      if (parent.isEmpty) 0
      else calculateLevel(ents, parent.get) + 1
    }
  }

  def printAnalysis(corpus: TrainingCorpus.Translations) {
    var entities = 0
    var alignedEntities = 0
    var deps = 0
    val depth = mutable.ArrayBuffer.empty[Int]
    val children = mutable.ArrayBuffer.empty[Int]
    val tokens = mutable.ArrayBuffer.empty[Int]
    var alignedSentences = 0

    corpus.translations.zipWithIndex.foreach { case (tr, idx) =>
      val tks = tr.source.tokens.map(_.orth).mkString(" ")
      println(s"${idx + 1}: $tks")

      deps += tr.source.entities.find(_.dependency.isDefined).size

      val alignments = TrainingCorpus.alignments(tr)

      if (alignments.exists(_._2.isDefined)) alignedSentences += 1

      alignments.foreach { case (src, tgt) =>
        entities += 1
        if (tgt.isDefined) alignedEntities += 1

        tokens += tr.source.tokens.size
        depth += calculateLevel(tr.source.entities, src.entity)
        children += tr.source.entities.count(_.parent == src.entity.id)
      }
    }

    println(s"Sentences: ${corpus.translations.size}")
    println(s"Aligned sentences: $alignedSentences")
    println(s"Total entities: $entities")
    println(s"Aligned entities: $alignedEntities")
    println(s"Dependencies: $deps")

    def avg(values: Seq[Int]) = values.sum.toDouble / values.size

    println(s"Children (max): ${children.max}")
    println(s"Children (avg): ${avg(children)}")
    println(s"Depth (max): ${depth.max}")
    println(s"Depth (avg): ${avg(depth)}")
    println(s"Tokens (max): ${tokens.max}")
    println(s"Tokens (avg): ${avg(tokens)}")
  }

  def main(args: Array[String]) {
    val alignmentsPath = "../parallel-de-pl/alignments.xml"
    val corpus = TrainingCorpus.read(alignmentsPath)
    printAnalysis(corpus)
  }
}

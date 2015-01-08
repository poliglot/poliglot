package poliglot.tools

import java.io.FileWriter

import poliglot.formats.TrainingCorpus.ResolvedEntity
import poliglot.formats.{TabDictionary, Dictcc, TrainingCorpus}
import poliglot.languages.{Polish, German}

import scala.collection.mutable

/* TODO Perhaps we can also take into account other entities or tokens on the
 * same level for higher accuracy (e.g. POS of previous word)?
 */
object DeriveAlignments {
  case class Features(dictccPath: String, lemmasPath: String) {
    val dict = (Dictcc.read(dictccPath) ++ TabDictionary.read(lemmasPath))
      .map { case (src, tgt) => (src.toLowerCase, tgt.toLowerCase) }

    val offsetRatio: QueryFeature = { case (_, src, tgt) =>
      val a = src.entity.tokens._1 + 1
      val b = tgt.entity.tokens._1 + 1
      a.toDouble / b
    }

    val lengthRatio: QueryFeature = { case (_, src, tgt) =>
      src.tokens.length / tgt.tokens.length
    }

    val posSimilarity: QueryFeature = { case (_, src, tgt) =>
      val srcTags = src.tokens.map(_.tag.split(':')).map(German.partOfSpeech(_))
      val tgtTags = tgt.tokens.map(_.tag.split(':')).map(Polish.partOfSpeech(_))

      val tgtQueue = mutable.ArrayBuffer(tgtTags: _*)

      srcTags.map { cur =>
        if (tgtQueue.contains(cur)) {
          tgtQueue -= cur
          1
        } else 0
      }.sum.toDouble / srcTags.size
    }

    val orthSimilarity: QueryFeature = { case (tr, src, tgt) =>
      val srcTags = src.tokens.map(_.orth)
      val tgtTags = tgt.tokens.map(_.orth)

      val tgtQueue = mutable.ArrayBuffer(tgtTags: _*)

      srcTags.map { cur =>
        if (tgtQueue.contains(cur)) {
          tgtQueue -= cur
          1
        } else 0
      }.sum.toDouble / srcTags.size
    }

    val lemmaSimilarity: QueryFeature = { case (tr, src, tgt) =>
      val srcTags = src.tokens.map(_.lemma)
      val tgtTags = tgt.tokens.map(_.lemma)

      val tgtQueue = mutable.ArrayBuffer(tgtTags: _*)

      srcTags.map { cur =>
        if (tgtQueue.contains(cur)) {
          tgtQueue -= cur
          1
        } else 0
      }.sum.toDouble / srcTags.size
    }

    val dictCorrespondences: QueryFeature = { case (tr, src, tgt) =>
      src.tokens.map { tk =>
        val found = tgt.tokens.exists { tk2 =>
          dict.contains((tk.orth.toLowerCase, tk2.orth.toLowerCase)) ||
            dict.contains((tk.lemma.toLowerCase, tk2.lemma.toLowerCase))
        }
        if (found) 1 else 0
      }.sum.toDouble / src.tokens.size
    }

    val sourceChildren: QueryFeature = {
      case (tr, src, tgt) =>
        val srcChildren = tr.source.entities.count(_.parent == src.entity.id)
        srcChildren
    }

    val targetChildren: QueryFeature = {
      case (tr, src, tgt) =>
        val tgtChildren = tr.target.entities.count(_.parent == tgt.entity.id)
        tgtChildren
    }

    val childrenRatio: QueryFeature = {
      case (tr, src, tgt) =>
        val srcChildren = tr.source.entities.count(_.parent == src.entity.id)
        val tgtChildren = tr.target.entities.count(_.parent == tgt.entity.id)
        if (tgtChildren == 0) 0.0
        else srcChildren.toDouble / tgtChildren
    }

    def calculateLevel(ents: Seq[TrainingCorpus.Entity], ent: TrainingCorpus.Entity): Int = {
      if (ent.parent == -1) 0
      else {
        val parent = ents.find(_.id == ent.parent)
        if (parent.isEmpty) 0
        else calculateLevel(ents, parent.get) + 1
      }
    }

    val depthDiff: QueryFeature = {
      case (tr, src, tgt) =>
        val srcLevel = calculateLevel(tr.source.entities, src.entity)
        val tgtLevel = calculateLevel(tr.target.entities, tgt.entity)

        Math.abs(srcLevel - tgtLevel)
    }

    val depthRatio: QueryFeature = {
      case (tr, src, tgt) =>
        val srcLevel = calculateLevel(tr.source.entities, src.entity)
        val tgtLevel = calculateLevel(tr.target.entities, tgt.entity)

        if (tgtLevel == 0) 0.0
        else Math.abs(srcLevel.toDouble / tgtLevel)
    }

    val queryFeatures = Seq(
      (offsetRatio, "offsetRatio"),
      (lengthRatio, "lengthRatio"),
      (posSimilarity, "posSimilarity"),
      (orthSimilarity, "orthSimilarity"),
      (lemmaSimilarity, "lemmaSimilarity"),
      (dictCorrespondences, "dictCorrespondences"),
      (sourceChildren, "sourceChildren"),
      (targetChildren, "targetChildren"),
      (childrenRatio, "childrenRatio"),
      (depthDiff, "depthDiff"),
      (depthRatio, "depthRatio")
    )
  }

  type QueryFeature = TrainingCorpus.Feature[Double]

  case class Instance(tr: TrainingCorpus.Translation, source: ResolvedEntity, target: ResolvedEntity, `class`: Boolean)

  def instances(corpus: TrainingCorpus.Translations): Seq[Instance] = {
    val instances = mutable.ArrayBuffer.empty[Instance]

    corpus.translations.foreach { tr =>
      TrainingCorpus.alignments(tr).foreach { case (src, tgt) =>
        if (tgt.isDefined) {
          assert(tgt.get.tokens.nonEmpty)
          instances += Instance(tr, src, tgt.get, `class` = true)
        }

        tr.target.entities
          .filter(ent => !tgt.exists(_.entity == ent))
          .foreach
        { other =>
          val resolved = TrainingCorpus.resolve(other, tr.target)
          assert(resolved.tokens.nonEmpty)
          instances += Instance(tr, src, resolved, `class` = false)
        }
      }
    }

    instances
  }

  def writeArff(queryFeatures: Seq[(QueryFeature, String)],
                insts: Seq[Instance],
                outputPath: String)
  {
    val out = new FileWriter(outputPath)
    out.write("@relation Alignment\n")

    queryFeatures.foreach { case (f, s) => out.write(s"@attribute $s numeric\n") }

    out.write(s"@attribute class {0.0, 1.0}\n")
    out.write("@data\n")

    insts.zipWithIndex.foreach { case (Instance(tr, src, tgt, good), i) =>
      val values = queryFeatures.map(_._1(tr, src, tgt)) :+
        (if (good) 1.0 else 0.0)
      out.write(values.mkString(",") + "\n")
    }

    out.close()
  }

  def trainCorpus(features: Features, corpus: TrainingCorpus.Translations) {
    val insts = instances(corpus)
    writeArff(features.queryFeatures, insts, "/tmp/align.arff")
  }

  def run(dictccPath: String, lemmasPath: String, alignmentsPath: String) {
    val corpus = TrainingCorpus.read(alignmentsPath)
    val features = Features(dictccPath, lemmasPath)
    trainCorpus(features, corpus)
  }

  def main(args: Array[String]) {
    run(
      "../parallel-de-pl/dictcc-de-pl.txt",
      "../parallel-de-pl/lemmas.txt",
      "../parallel-de-pl/alignments.xml"
    )
  }
}

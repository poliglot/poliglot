package poliglot.tools

import java.io.FileWriter

import poliglot.formats.TrainingCorpus.FeatureOpt
import poliglot.{POS, MorphAttribute}
import poliglot.languages.{Polish, German}
import poliglot.formats.TrainingCorpus

import scala.collection.mutable

object AnalyseAdpositions {
  type Feature = TrainingCorpus.FeatureOpt[String]
  type Features = Seq[(Feature, String)]

  val deLemma: Feature = { case (tr, src, _) =>
    src.tokens.head.lemma.toLowerCase
  }

  val dePOS: Feature = { case (tr, src, _) =>
    German.partOfSpeech(src.tokens.head.tag.split(':')).toString
  }

  val deDependencyLemma: Feature = { case (tr, src, _) =>
    src.entity.dependency.map { dep =>
      val entity = tr.source.entities.find(_.id == dep).get
      val resolved = TrainingCorpus.resolve(entity, tr.source)
      val tk = resolved.tokens.head
      tk.lemma
    }.getOrElse("none")
  }

  /* POS tag of token that the adposition depends on */
  val deDependencyPOS: Feature = { case (tr, src, _) =>
    src.entity.dependency.map { dep =>
      val entity = tr.source.entities.find(_.id == dep).get
      val resolved = TrainingCorpus.resolve(entity, tr.source)
      val tk = resolved.tokens.head
      German.partOfSpeech(tk.tag.split(':')).toString
    }.getOrElse("none")
  }

  val deArgumentPOS: Feature = { case (tr, src, _) =>
    val deps = tr.source.entities.filter(_.dependency.contains(src.entity.id))

    deps.headOption.map { dep =>
      val entity = deps(0)
      val resolved = TrainingCorpus.resolve(entity, tr.source)
      val tk = resolved.tokens.head
      German.partOfSpeech(tk.tag.split(':')).toString
    }.getOrElse("none")
  }

  /** RFTagger does not associate the case to adpositions, but the dependent
    * entity contains this information.
    */
  val deArgumentCase: Feature = { case (tr, src, _) =>
    val deps = tr.source.entities.filter(_.dependency.contains(src.entity.id))

    deps.headOption.flatMap { dep =>
      val resolved = TrainingCorpus.resolve(dep, tr.source)
      val tk = resolved.tokens.head
      German.morphAttributes(tk.orth, tk.tag.split(':'))
        .find(_.isInstanceOf[MorphAttribute.Case])
        .map(_.toString)
    }.getOrElse("none")
  }

  val plLemma: Feature = { case (_, _, tgt) =>
    tgt
      .map(_.tokens.head.lemma.toLowerCase)
      .getOrElse("none")
  }

  val plPOS: Feature = { case (_, _, tgt) =>
    tgt.map { ent =>
      val prep = ent.tokens.headOption.get
      Polish.partOfSpeech(prep.tag.split(':')).toString
    }.getOrElse("none")
  }

  val plCase: Feature = { case (_, _, tgt) =>
    tgt.flatMap { ent =>
      val prep = ent.tokens.headOption.get
      Polish.morphAttributes(prep.orth, prep.tag.split(':'))
        .find(_.isInstanceOf[MorphAttribute.Case])
        .map(_.toString)
    }.getOrElse("none")
  }

  val plArgumentPOS: Feature = { case (tr, src, tgt) =>
    val deps = tr.source.entities.filter(_.dependency.contains(src.entity.id))

    deps.find(_ != src.entity).flatMap { dep =>
      dep.alignment.map { align =>
        val alignedEntity = tr.target.entities.find(_.id == align).get
        val resolved = TrainingCorpus.resolve(alignedEntity, tr.target)
        val tk = resolved.tokens.head
        Polish.partOfSpeech(tk.tag.split(':')).toString
      }
    }.getOrElse("none")
  }

  val deFeatures: Features = Seq(
    (deLemma, "deLemma")
    , (dePOS, "dePOS")
    , (deArgumentPOS, "deArgumentPOS")
    , (deArgumentCase, "deArgumentCase")
    , (deDependencyLemma, "deDependencyLemma")
    , (deDependencyPOS, "deDependencyPOS")
  )

  val plFeatures: Features = Seq(
    (plLemma, "plLemma")
    , (plPOS, "plPOS")
    , (plCase, "plCase")
    , (plArgumentPOS, "plArgumentPOS")
  )

  val allFeatures = deFeatures ++ plFeatures

  def instances(corpus: TrainingCorpus.Translations,
                features: Features): Seq[String] =
  {
    corpus.translations.zipWithIndex.flatMap { case (tr, i) =>
      println(s"tr = $i")
      val alignments = TrainingCorpus.alignments(tr)

      alignments.flatMap { case (src, tgt) =>
        val `class` = src.entity.`class`
        if (`class`.nonEmpty) {
          /* If tagged correctly, there must only one token in a preposition entity. */
          assert(src.tokens.size == 1)

          val values = features.map { case (f, _) => f(tr, src, tgt) }.mkString(",")
          println(s"values = $values")
          List(s"$values,${`class`}\n")
        } else List()
      }
    }
  }

  case class Instance(features: Seq[String], `class`: String)

  def writeArff(features: Seq[(FeatureOpt[String], String)],
                insts: Seq[Instance],
                outputPath: String)
  {
    val out = new FileWriter(outputPath)
    out.write("@relation Adpositions\n")

    features.zipWithIndex.foreach { case (ft, i) =>
      val values = insts.map(_.features(i)).toSet.mkString(",")
      val ftName = ft._2
      out.write(s"@attribute $ftName {$values}\n")
    }

    val classValues = insts.map(_.`class`).toSet.mkString(",")
    out.write(s"@attribute class {$classValues}\n")

    out.write("@data\n")
    insts.zipWithIndex.foreach { case (Instance(feats, clazz), i) =>
      out.write((feats :+ clazz).mkString(",") + "\n")
    }

    out.close()
  }

  def wekaInstances(corpus: TrainingCorpus.Translations,
                    features: Features): Seq[Instance] =
  {
    corpus.translations.zipWithIndex.flatMap { case (tr, i) =>
      val alignments = TrainingCorpus.alignments(tr)

      alignments.flatMap { case (src, tgt) =>
        /* Even though the bilingual corpus comprises more adpositions, these are
         * filtered out because they are not guaranteed to have enough instances.
         */
        val isAdposition = German.Adpositions
          .contains(src.tokens.head.lemma.toLowerCase)
        val `class` = src.entity.`class`

        if (isAdposition && `class`.nonEmpty) {
          /* If tagged correctly, there must only one token in a preposition entity. */
          assert(src.tokens.size == 1)

          val values = features.map { case (f, _) => f(tr, src, tgt) }
          List(Instance(values, `class`))
        } else List()
      }
    }
  }

  def trainCorpus(corpus: TrainingCorpus.Translations, features: Features, path: String) {
    val insts = wekaInstances(corpus, features)
    writeArff(features, insts, path)
  }

  def adposCorresp(corpus: TrainingCorpus.Translations): mutable.Set[(String, String)] = {
    val adpositions = mutable.HashSet.empty[(String, String)]

    corpus.translations.foreach { tr =>
      val alignments = TrainingCorpus.alignments(tr)

      alignments.foreach { case (src, tgt) =>
        val adposition = src.tokens.head.lemma.toLowerCase
        val isAdposition = German.Adpositions.contains(adposition)
        val `class` = src.entity.`class`
        if (isAdposition && `class`.nonEmpty) {
          tgt.foreach { t =>
            if (List(POS.Adposition, POS.Clitic, POS.Particle, POS.Conjunction)
              .contains(Polish.partOfSpeech(t.tokens.head.tag.split(':'))))
            {
              adpositions += (adposition -> t.tokens.head.lemma.toLowerCase)
            }
          }
        }
      }
    }

    adpositions
  }

  def caseCorresp(corpus: TrainingCorpus.Translations): mutable.Set[(String, String)] = {
    val caseMapping = mutable.HashSet.empty[(String, String)]

    corpus.translations.foreach { tr =>
      val alignments = TrainingCorpus.alignments(tr)

      alignments.foreach { case (src, tgt) =>
        val adposition = src.tokens.head.lemma.toLowerCase
        val isAdposition = German.Adpositions.contains(adposition)
        val `class` = src.entity.`class`
        if (isAdposition && `class`.nonEmpty) {
          caseMapping += (deArgumentCase(tr, src, tgt) -> plCase(tr, src, tgt))
        }
      }
    }

    caseMapping
  }

  def classesToPreps(corpus: TrainingCorpus.Translations): mutable.Map[String, (mutable.Set[String], mutable.Set[String])] = {
    val mapping = mutable.HashMap.empty[String, (mutable.Set[String], mutable.Set[String])]

    corpus.translations.foreach { tr =>
      val alignments = TrainingCorpus.alignments(tr)

      alignments.foreach { case (src, tgt) =>
        val adposition = src.tokens.head.lemma.toLowerCase
        val isAdposition = German.Adpositions.contains(adposition)
        val `class` = src.entity.`class`
        if (isAdposition && `class`.nonEmpty) {
          if (!mapping.isDefinedAt(`class`))
            mapping += (`class` -> (mutable.Set.empty[String], mutable.Set.empty[String]))

          mapping(`class`)._1 += adposition

          tgt.foreach { t =>
            if (List(POS.Adposition, POS.Clitic, POS.Particle, POS.Conjunction)
              .contains(Polish.partOfSpeech(t.tokens.head.tag.split(':'))))
            {
              mapping(`class`)._2 += t.tokens.head.lemma.toLowerCase
            }
          }
        }
      }
    }

    mapping
  }

  def counts(corpus: TrainingCorpus.Translations): mutable.Map[String, Int] = {
    val adpositions = mutable.HashMap.empty[String, Int].withDefaultValue(0)

    corpus.translations.foreach { tr =>
      val alignments = TrainingCorpus.alignments(tr)

      alignments.foreach { case (src, tgt) =>
        val adposition = src.tokens.head.lemma.toLowerCase
        val isAdposition = German.Adpositions.contains(adposition)
        val `class` = src.entity.`class`
        if (isAdposition && `class`.nonEmpty) {
          adpositions(adposition) += 1
        }
      }
    }

    adpositions
  }

  def run(alignmentsPath: String) {
    val corpus = TrainingCorpus.read(alignmentsPath)

    println("Adposition correspondences")
    println(adposCorresp(corpus).groupBy(_._1).toList.sortBy(_._1).map { case (k,v) =>
      (k, v.map(_._2).toList.sorted)
    }.mkString("\n"))

    println("Case correspondences")
    println(caseCorresp(corpus).groupBy(_._1).toList.sortBy(_._1).map { case (k,v) =>
      (k, v.map(_._2).toList.sorted)
    }.mkString("\n"))

    println("Classes to adpositions")
    println(classesToPreps(corpus).toList.sortBy(_._1).map { case (k, v) =>
      val de = v._1.toList.sorted.mkString(", ")
      val pl = v._2.toList.sorted.mkString(", ")
      s"| $k | $de | $pl |"
    }.mkString("\n"))

    println("Adposition counts")
    println(counts(corpus).toList.sortBy(_._2).mkString("\n"))

    println(trainCorpus(corpus, deFeatures, "/tmp/adpositions-de.arff"))
    println(trainCorpus(corpus, plFeatures, "/tmp/adpositions-pl.arff"))
    println(trainCorpus(corpus, allFeatures, "/tmp/adpositions-de-pl.arff"))
  }

  def main(args: Array[String]) {
    run(
      "../parallel-de-pl/alignments.xml"
    )
  }
}

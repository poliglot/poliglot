package poliglot.formats

import scala.xml.{NodeSeq, XML}

/** Parallel training corpus capturing entities, alignments and class tags. */
object TrainingCorpus {
  case class Token(orth: String, lemma: String, tag: String)
  case class Entity(id: Int,
                    parent: Int,
                    tokens: (Int, Int),
                    dependency: Option[Int], /* ID this entity semantically depends on. */
                    alignment: Option[Int], /* ID of target entity. */
                    `class`: String)
  case class Tokens(tokens: Seq[Token], entities: Seq[Entity])
  case class Translation(source: Tokens, target: Tokens, done: Boolean)
  case class Translations(translations: Seq[Translation])

  case class ResolvedEntity(entity: Entity, tokens: Seq[Token])

  type Feature[T] = (Translation, ResolvedEntity, ResolvedEntity) => T
  type FeatureOpt[T] = (Translation, ResolvedEntity, Option[ResolvedEntity]) => T

  def read(path: String): Translations = {
    val xml = XML.loadFile(path)

    def parseToken(node: NodeSeq) =
      Token(
        orth = (node \\ "orth").headOption.map(_.text.trim).getOrElse(""),
        lemma = (node \\ "lemma").headOption.map(_.text.trim).getOrElse(""),
        tag = (node \\ "tag").headOption.map(_.text.trim).getOrElse("")
      )

    def parseEntity(node: NodeSeq) =
      Entity(
        id = (node \\ "id").headOption.map(_.head.text.toInt).get,
        parent = (node \\ "parent").headOption.map(_.head.text.toInt).get,
        tokens = (
          (node \\ "startToken").headOption.map(_.head.text.toInt).get,
          (node \\ "endToken").headOption.map(_.head.text.toInt).get
        ),
        dependency = (node \\ "dependency").headOption.flatMap { node =>
          val value = node.head.text.toInt
          if (value == -1) None else Some(value)
        },
        alignment = (node \\ "alignment").headOption.flatMap { node =>
          val value = node.head.text.toInt
          if (value == -1) None else Some(value)
        },
        `class` = (node \\ "class").headOption.map(_.head.text).getOrElse("")
      )

    def parseTokens(node: NodeSeq) =
      Tokens(
        tokens = (node \\ "tokens" \\ "token").map(parseToken),
        entities = (node \\ "entities" \\ "entity").map(parseEntity)
      )

    def parseTranslation(node: NodeSeq) =
      Translation(
        source = parseTokens(node \\ "source"),
        target = parseTokens(node \\ "target"),
        done = (node \\ "done").headOption.exists(_.head.text == "true")
      )

    Translations((xml \\ "translation").map(parseTranslation))
  }

  /** For each source entity, find an invalid target entity. */
  def badAlignments(source: Seq[TrainingCorpus.Tokens],
                 target: Seq[TrainingCorpus.Tokens]):
      Seq[(ResolvedEntity, ResolvedEntity)] =
    source.zip(target).flatMap { case (src, tgt) =>
      val potentialTargetEntities =
        for { a <- 0 until tgt.tokens.size
              b <- (a + 1) until tgt.tokens.size } yield (a, b)

      src.entities.filter(_.alignment.isDefined).map { srcEntity =>
        val tgtEntity = tgt.entities.find(_.id == srcEntity.alignment.get).get
        val badEntities = potentialTargetEntities.diff(Seq(tgtEntity.tokens))
        val randomEntity = util.Random.shuffle(badEntities).head

        val srcTokens = (srcEntity.tokens._1 until srcEntity.tokens._2)
          .map(src.tokens)
        val tgtTokens = (randomEntity._1 until randomEntity._2)
          .map(tgt.tokens)

        (ResolvedEntity(srcEntity, srcTokens),
          ResolvedEntity(tgtEntity, tgtTokens))
      }
    }

  type AlignedEntities = Seq[(ResolvedEntity, Option[ResolvedEntity])]

  def resolve(ent: Entity, tks: Tokens): ResolvedEntity =
    ResolvedEntity(ent, (ent.tokens._1 until ent.tokens._2).map(tks.tokens))

  def alignments(source: Seq[TrainingCorpus.Tokens],
                 target: Seq[TrainingCorpus.Tokens]): Seq[AlignedEntities] =
    source.zip(target).map { case (src, tgt) =>
      src.entities.map { srcEntity =>
        val srcTokens = (srcEntity.tokens._1 until srcEntity.tokens._2)
          .map(src.tokens)

        if (srcEntity.alignment.isDefined) {
          val tgtEntity = tgt.entities.find(_.id == srcEntity.alignment.get).get
          val tgtTokens = (tgtEntity.tokens._1 until tgtEntity.tokens._2)
            .map(tgt.tokens)

          (ResolvedEntity(srcEntity, srcTokens),
           Some(ResolvedEntity(tgtEntity, tgtTokens)))
        } else {
          (ResolvedEntity(srcEntity, srcTokens), None)
        }
      }
    }

  def alignments(tr: Translation): AlignedEntities =
    tr.source.entities.map { srcEntity =>
      val srcTokens = (srcEntity.tokens._1 until srcEntity.tokens._2)
        .map(tr.source.tokens)

      if (srcEntity.alignment.isDefined) {
        val tgtEntity = tr.target.entities.find(_.id == srcEntity.alignment.get).get
        val tgtTokens = (tgtEntity.tokens._1 until tgtEntity.tokens._2)
          .map(tr.target.tokens)

        (ResolvedEntity(srcEntity, srcTokens),
          Some(ResolvedEntity(tgtEntity, tgtTokens)))
      } else {
        (ResolvedEntity(srcEntity, srcTokens), None)
      }
    }
}

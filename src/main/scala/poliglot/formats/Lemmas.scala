package poliglot.formats

import java.io.File

import scala.collection.mutable

case class Lemma(source: String, target: String)

object Lemmas {
  def read(path: String): mutable.HashSet[Lemma] = {
    val lemmas = mutable.HashSet.empty[Lemma]

    val file = io.Source.fromFile(new File(path))

    file.getLines().foreach { line =>
      val parts = line.split("\t")
      lemmas += Lemma(parts(0), parts(1))
    }

    file.close()

    lemmas
  }
}

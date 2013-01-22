package edu.washington.cs.knowitall
package tool
package coref

import collection.immutable.Interval
import edu.washington.cs.knowitall.tool.postag.Postagger

/*
 * A coreference resolver takes text as input and produces clusters
 * of mentions with the same target or resolves mentions with the
 * most informative mention. */
abstract class CoreferenceResolver {
  /*
   * Process a document and return a map of mentions in a cluster, where the
   * key is the most representitive mention. */
  def clusters(text: String): Map[Mention, List[Mention]]

  /*
   * Process a document and return a map of mention resolutions, where the
   * key set covers all mentions and the value is the most representitive
   * mention.  Mentions are ordered by offset.*/
  def substitutions(text: String): Seq[Substitution] = {
    val transform = for {
      (bestMention, mentions) <- clusters(text)
      mention <- mentions
      if mention.text != bestMention
    } yield (Substitution(mention, bestMention))

    transform.toSeq.sortBy(_.mention.offset)
  }
}

object CoreferenceResolver {
  /*
   * Process a document and return a document with all mentions replaced by
   * the most representitive mention. */
  def substitute(text: String, substitutions: Seq[Substitution]): String = {
    this.substituteBase(text, substitutions.map(sub => (sub.mention.charInterval, sub.best.text)))
  }

  /*
   * Process a document and return a document with all mentions replaced by
   * the most representitive mention. */
  def substituteBase(text: String, substitutions: Seq[(Interval, String)]): String = {
    var result = text
    val subs = substitutions.sortBy(_._1)

    var adjust = 0
    for ((interval, string) <- subs) {
      val shifted = interval.shift(adjust)
      result = result.take(shifted.start) + string + result.drop(shifted.end)
      adjust += string.size - interval.size
    }

    result
  }

  def substitute(indexedText: Seq[(Char, Int)], substitutions: Seq[(Interval, String)]): Seq[(Char, Int)] = {
    var result = indexedText
    val subs = substitutions.sortBy(_._1)

    var adjust = 0
    for ((interval, string) <- subs) {
      val shifted = interval.shift(adjust)
      val indexedBest = string.zipWithIndex.map {
        case (c, i) =>
          val shiftedIndex = i + interval.start
          val index =
            if (shiftedIndex > interval.last) interval.last
            else shiftedIndex
          (c, index)
      }
      result = result.takeWhile(_._2 < interval.start) ++ indexedBest ++ result.dropWhile(_._2 < interval.end)
      adjust += string.size - interval.size
    }

    result
  }

  def resolve(text: String, substitutions: Seq[Substitution]): Seq[ResolutionString] = {
    var result = Seq.empty[ResolutionString]
    var string = text
    val subs = substitutions.sortBy(_.mention.offset)

    var adjust = 0
    for (Substitution(original, resolution) <- subs) {
      val shifted = original.charInterval.shift(-adjust)
      result = result :+ NormalString(string.take(shifted.start)) :+ ResolvedString(original.text, resolution.text)
      string = string.drop(shifted.end)
      adjust += shifted.end
    }

    (result :+ NormalString(string)).filter(!_.string.isEmpty)
  }

  sealed abstract class ResolutionString {
    def string: String
  }
  case class ResolvedString(original: String, resolution: String) extends ResolutionString {
    override def string = resolution
  }
  case class NormalString(override val string: String) extends ResolutionString
}

/*
 * A representation for a mention in a document. */
case class Mention(text: String, offset: Int) {
  def normalized = text.toLowerCase.trim
  override def toString = offset + ":" + "\"" + text + "\""

  def charInterval = Interval.open(offset, offset + text.size)

  def isPronoun = {
    Postagger.pronouns contains text
  }


  def possessive = {
    normalized.endsWith("'s") || normalized.endsWith("s'") || Mention.possessives.exists(normalized endsWith _);
  }
}

object Mention {
  val possessives = Postagger.possessivePronouns ++ Postagger.possessives

  def isPossessive(word: String) =
    word.endsWith("'s") || word.endsWith("s'") || possessives.contains(word.trim.toLowerCase);
}

case class Substitution(mention: Mention, best: Mention) {
  def shift(shift: Int) = {
    new Substitution(
      this.mention.copy(offset = this.mention.offset + shift),
      this.best.copy(offset = this.best.offset + shift))
  }
}

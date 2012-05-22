package edu.washington.cs.knowitall
package tool
package stem

import common.main.LineProcessor

/** A stemmer takes a string token and produces a normalized form. */
abstract class Stemmer {
  /** Apply the stemming algorithm. */
  def stem(word: String): String

  /** Apply the stemming algorithm and then the normalizing algorithm. */
  def lemmatize(word: String) = this.stem(Stemmer.normalize(word))
}

object Stemmer {
  /** The default stemmer is the identity stemmer.
    * While not the most useful, it doesn't require any dependencies.
    * You usually would supply an alternate stemmer as an implicit parameter
    * where needed.
    */
  implicit def stemmer: Stemmer = IdentityStemmer

  /** Special characters to remove. */
  val remove = """[()\[\].,;:"']""".r;

  /** Remove special characters and lowercase the string. */
  def normalize(word: String) = Stemmer.remove.replaceAllIn(
    word.trim.toLowerCase, "")
}

abstract class StemmerMain
extends LineProcessor {
  def stemmer: Stemmer
  override def process(line: String) = line.split("\\s+").map(stemmer.lemmatize(_)).mkString(" ")
}

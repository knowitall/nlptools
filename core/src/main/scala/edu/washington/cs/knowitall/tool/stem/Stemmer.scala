package edu.washington.cs.knowitall
package tool
package stem

import common.main.LineProcessor
import tokenize.Token

/** A stemmer takes a string token and produces a normalized form. */
abstract class Stemmer {
  /** Apply the stemming algorithm. */
  def stem(word: String): String
  
  def stemToken[T <: Token](token: T) = Lemmatized(token, this.stem(token.string))

  /** Apply the stemming algorithm and then the normalizing algorithm. */
  def lemmatize(word: String) = this.stem(Stemmer.normalize(word))
  
  def lemmatizeToken[T <: Token](token: T) = Lemmatized(token, this.lemmatize(token.string))
}

object Stemmer {
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

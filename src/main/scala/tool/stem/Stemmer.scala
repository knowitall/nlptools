package edu.washington.cs.knowitall
package tool
package stem

import common.main.LineProcessor

abstract class Stemmer {
  def stem(word: String): String
  def normalize(word: String) = Stemmer.remove.replaceAllIn(word.trim.toLowerCase, "")
  def lemmatize(word: String) = this.stem(this.normalize(word))
}

object Stemmer {
  val remove = """[()\[\].,;:"']""".r;
}

abstract class StemmerMain
extends LineProcessor {
  def stemmer: Stemmer
  override def process(line: String) = line.split("\\s+").map(stemmer.lemmatize(_)).mkString(" ")
}

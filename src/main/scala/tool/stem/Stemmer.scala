package edu.washington.cs.knowitall
package tool
package stem

import common.main.LineProcessor

abstract class Stemmer {
  def stem(word: String): String
  def lemmatize(word: String) = this.stem(word).toLowerCase
}

abstract class StemmerMain
extends LineProcessor {
  def stemmer: Stemmer
  override def process(line: String) = line.split("\\s+").map(stemmer.stem(_)).mkString(" ")
}

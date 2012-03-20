package edu.washington.cs.knowitall
package tool
package stem

import java.io.StringReader

import org.tartarus.{PorterStemmer => Porter}
import common.main.LineProcessor

class PorterStemmer extends Stemmer {
  val stemmer = new Porter()

  def stem(word: String) = stemmer.stem(word)
}

object PorterStemmer
extends StemmerMain {
  lazy val stemmer = new PorterStemmer
}

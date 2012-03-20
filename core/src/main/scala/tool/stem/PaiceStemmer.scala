package edu.washington.cs.knowitall
package tool
package stem

import java.io.StringReader

import uk.ac.lancs.comp.{PaiceStemmer => Paice}
import common.main.LineProcessor

class PaiceStemmer extends Stemmer {
  val stemmer = new Paice()

  def stem(word: String) = stemmer.stripAffixes(word)
}

object PaiceStemmer
extends StemmerMain {
  lazy val stemmer = new PaiceStemmer
}

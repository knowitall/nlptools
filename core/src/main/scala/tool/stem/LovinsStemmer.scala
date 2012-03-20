package edu.washington.cs.knowitall
package tool
package stem

import java.io.StringReader

import uk.ac.lancs.comp.{LovinsStemmer => Lovins}
import common.main.LineProcessor

class LovinsStemmer extends Stemmer {
  val stemmer = new Lovins()

  def stem(word: String) = stemmer.stem(word.toLowerCase)
}

object LovinsStemmer
extends StemmerMain {
  lazy val stemmer = new LovinsStemmer
}

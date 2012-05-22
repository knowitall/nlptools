package edu.washington.cs.knowitall
package tool
package stem

import org.tartarus.snowball.SnowballProgram

class SnowballStemmer(stemmer: SnowballProgram) extends Stemmer {
  def stem(word: String) = {
    stemmer.setCurrent(word)

    if (stemmer.stem()) {
      stemmer.getCurrent
    }
    else {
      word
    }
  }
}

/*
object SnowballStemmer
extends StemmerMain {
  lazy val stemmer = new PorterStemmer
}
*/

package edu.knowitall
package tool
package stem

import org.tartarus.snowball.SnowballProgram

/** Wrapper for any Snowball stemmer. */
class SnowballStemmer(stemmer: SnowballProgram) extends Stemmer {
  def stem(word: String) = {
    stemmer.setCurrent(word)

    if (stemmer.stem()) {
      stemmer.getCurrent
    } else {
      word
    }
  }
}

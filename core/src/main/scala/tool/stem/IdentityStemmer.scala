package edu.washington.cs.knowitall
package tool
package stem

import common.main.LineProcessor

/**
  * A trivial stemmer that doesn't apply a stemming algorithm. 
  */
object IdentityStemmer extends Stemmer {
  override def stem(word: String) = word
}

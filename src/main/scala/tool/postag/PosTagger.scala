package edu.washington.cs.knowitall
package tool
package postag

import edu.washington.cs.knowitall._

abstract class PosTagger(val tokenizer: tokenize.Tokenizer) {
  def postag(tokens: Array[String]): Array[String]

  def postag(sentence: String): IndexedSeq[(String, String)] = {
    val tokens = tokenizer.tokenize(sentence)
    val tags = postag(tokens)
    tokens zip tags
  }
}

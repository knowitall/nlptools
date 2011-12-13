package edu.washington.cs.knowitall
package tool
package postag

import edu.washington.cs.knowitall._

/* A POS tagger takes tokenized input and associates a part of speech
 * tag with each token. */
abstract class PosTagger(val tokenizer: tokenize.Tokenizer) {
  /* POS tag pre-tokenized text */
  def postag(tokens: Array[String]): Array[String]

  /* Tokenize and then POS tag text*/
  def postag(sentence: String): IndexedSeq[(String, String)] = {
    val tokens = tokenizer.tokenize(sentence)
    val tags = postag(tokens)
    tokens zip tags
  }
}

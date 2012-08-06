package edu.washington.cs.knowitall
package tool
package tokenize

/** A tokenizer takes a sentence string as input and
  * seperates words (tokens) along word (token) boundaries.
  */
trait Tokenizer {
  def tokenize(sentence: String): Seq[Token]
}

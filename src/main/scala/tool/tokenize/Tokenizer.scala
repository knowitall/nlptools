package edu.washington.cs.knowitall
package tool
package tokenize

trait Tokenizer {
  def tokenize(sentence: String): Array[String]
}

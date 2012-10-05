package edu.washington.cs.knowitall
package tool
package tokenize

import scala.collection.JavaConversions._
import breeze.text.tokenize.{ PTBTokenizer => BreezePTBTokenizer }

class PTBTokenizer extends Tokenizer {
  val tokenizer = BreezePTBTokenizer()

  def tokenize(sentence: String): Seq[Token] = {
    val strings = tokenizer(sentence)
    Tokenizer.computeOffsets(strings, sentence)
  }
}

object PTBTokenizerMain extends LineProcessor {
  val tokenizer = new PTBTokenizer()
  override def process(sentence: String) =
    tokenizer.tokenize(sentence).mkString(" ")
}

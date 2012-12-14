package edu.washington.cs.knowitall
package tool
package tokenize

import scala.collection.JavaConversions._
import breeze.text.tokenize.{ SimpleEnglishTokenizer => BreezeEnglishTokenizer }

class SimpleEnglishTokenizer extends Tokenizer {
  val tokenizer = BreezeEnglishTokenizer()

  def tokenize(sentence: String): Seq[Token] = {
    val strings = tokenizer(sentence)
    Tokenizer.computeOffsets(strings, sentence)
  }
}

object SimpleEnglishTokenizerMain extends LineProcessor {
  val tokenizer = new PTBTokenizer()
  override def process(sentence: String) =
    tokenizer.tokenize(sentence).mkString(" ")
}

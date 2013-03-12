package edu.knowitall
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

object SimpleEnglishTokenizerMain extends TokenizerMain {
  val tokenizer = new SimpleEnglishTokenizer()
}

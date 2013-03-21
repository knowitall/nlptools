package edu.knowitall
package tool
package tokenize

import scala.collection.JavaConversions._

import edu.stanford.nlp.ling._;
import edu.stanford.nlp.process._;

class StanfordTokenizer extends Tokenizer {
  def tokenize(sentence: String) =
    new PTBTokenizer(
      new java.io.StringReader(sentence),
      new CoreLabelTokenFactory(),
      "").map { stoken =>
    new Token(stoken.word, stoken.beginPosition())
  }.toList
}

object StanfordTokenizerMain extends TokenizerMain {
  val tokenizer = new StanfordTokenizer()
}

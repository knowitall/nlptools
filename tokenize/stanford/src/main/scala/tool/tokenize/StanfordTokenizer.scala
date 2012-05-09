package edu.washington.cs.knowitall
package tool
package tokenize

import common.main.LineProcessor

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
  }.toSeq
}

object StanfordTokenizer extends LineProcessor {
  val tokenizer = new StanfordTokenizer()
  override def process(sentence: String) =
    tokenizer.tokenize(sentence).mkString(" ")
}

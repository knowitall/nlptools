package edu.washington.cs.knowitall
package tool
package tokenize

import common.main.LineProcessor

import collection.JavaConversions._

import opennlp.tools.tokenize._;

class OpenNlpTokenizer(val model: TokenizerModel) extends Tokenizer {
  def this(modelName: String = "en-token.bin") =
    this(new TokenizerModel(
      classOf[OpenNlpTokenizer].getClassLoader.getResourceAsStream(modelName)))

  val tokenizer = new TokenizerME(model)

  def tokenize(sentence: String) = tokenizer.tokenize(sentence)
}

object OpenNlpTokenizer extends LineProcessor {
  val tokenizer = new OpenNlpTokenizer()
  override def process(sentence: String) = 
    tokenizer.tokenize(sentence).mkString(" ")
}

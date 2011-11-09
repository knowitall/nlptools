package edu.washington.cs.knowitall
package tool
package postag

import common.main.LineProcessor

import opennlp.tools.postag._

class OpenNlpPosTagger(
  val model: POSModel,
  tokenizer: tokenize.Tokenizer)
extends PosTagger(tokenizer) {

  def this(
    modelName: String = "en-pos-maxent.bin", 
    tokenizer: tokenize.Tokenizer = new tokenize.OpenNlpTokenizer()) =
    this(new POSModel(
      classOf[OpenNlpPosTagger].getClassLoader.getResourceAsStream(modelName)),
      tokenizer)


  val tagger = new POSTaggerME(model)

  override def postag(tokens: Array[String]) =
    tagger.tag(tokens.toArray).toArray
}

object OpenNlpPosTagger extends LineProcessor {
  val tagger = new OpenNlpPosTagger()
  override def process(line: String) = tagger.postag(line).map{case (a,b) => a + "/" + b}.mkString(" ")
}

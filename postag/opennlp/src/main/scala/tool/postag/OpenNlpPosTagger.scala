package edu.washington.cs.knowitall
package tool
package postag

import common.main.LineProcessor
import opennlp.tools.postag._
import tool.tokenize.Token

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

  override def postagTokens(tokens: Seq[Token]): Seq[PostaggedToken] = {
    val postags = tagger.tag(tokens.iterator.map(_.string).toArray)
    (tokens zip postags).map { case (token, postag) =>
      new PostaggedToken(token, postag)
    }
  }
}

object OpenNlpPosTagger extends LineProcessor {
  val tagger = new OpenNlpPosTagger()
  override def process(line: String) =
    tagger.postag(line).map { case PostaggedToken(postag, string, offset) =>
      string + "/" + postag
    }.mkString(" ")
}
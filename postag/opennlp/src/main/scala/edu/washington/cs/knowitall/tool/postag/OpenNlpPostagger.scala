package edu.washington.cs.knowitall
package tool
package postag

import opennlp.tools.postag._
import tool.tokenize.Token

class OpenNlpPostagger(
  val model: POSModel,
  tokenizer: tokenize.Tokenizer)
extends Postagger(tokenizer) {

  def this(
    modelName: String = "en-pos-maxent.bin",
    tokenizer: tokenize.Tokenizer = new tokenize.OpenNlpTokenizer()) =
    this(new POSModel(
      classOf[OpenNlpPostagger].getClassLoader.getResourceAsStream(modelName)),
      tokenizer)


  val tagger = new POSTaggerME(model)

  override def postagTokens(tokens: Seq[Token]): Seq[PostaggedToken] = {
    val postags = tagger.tag(tokens.iterator.map(_.string).toArray)
    (tokens zip postags).map { case (token, postag) =>
      new PostaggedToken(token, postag)
    }
  }
}

object OpenNlpPostaggerMain extends PostaggerMain {
  override val tagger = new OpenNlpPostagger()
}

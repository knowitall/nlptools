package edu.knowitall
package tool
package postag

import scala.collection.JavaConversions._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import tool.tokenize.Token

class StanfordPostagger(
  val model: String = "edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger",
  tokenizer: tokenize.Tokenizer = new tokenize.StanfordTokenizer())
extends Postagger(tokenizer) {

  val tagger = new MaxentTagger(model)

  override def postagTokens(tokens: Seq[Token]): Seq[PostaggedToken] = {
    val postags = tagger.tagSentence(
      tokens.map { token =>
        val corelabel = new CoreLabel();
        corelabel.setWord(token.string);
        corelabel
      }.toList
    ).map(_.tag())

    (tokens zip postags) map { case (token, postag) =>
      new PostaggedToken(token, postag)
    }
  }
}

object StanfordPostaggerMain extends PostaggerMain {
  override val tagger = new StanfordPostagger()
}

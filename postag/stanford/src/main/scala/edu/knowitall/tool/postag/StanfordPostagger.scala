package edu.knowitall
package tool
package postag

import java.net.URL
import scala.collection.JavaConversions._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import tool.tokenize.Token

class StanfordPostagger(
  val tagger: MaxentTagger,
  tokenizer: tokenize.Tokenizer = new tokenize.StanfordTokenizer())
extends Postagger(tokenizer) {

  def this() = this(StanfordPostagger.loadDefaultModel())

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

object StanfordPostagger {
  def loadDefaultModel(): MaxentTagger = {
    new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")
  }
}

object StanfordPostaggerMain extends PostaggerMain {
  override val tagger = new StanfordPostagger()
}

package edu.washington.cs.knowitall
package tool
package postag

import common.main.LineProcessor
import scala.collection.JavaConversions._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import tool.tokenize.Token

class StanfordPosTagger(
  val model: String = "edu/stanford/nlp/models/pos-tagger/wsj3t0-18-left3words/left3words-distsim-wsj-0-18.tagger",
  tokenizer: tokenize.Tokenizer = new tokenize.StanfordTokenizer())
extends PosTagger(tokenizer) {

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

object StanfordPosTagger extends LineProcessor {
  val tagger = new StanfordPosTagger()
  override def process(line: String) =
    tagger.postag(line).map { case PostaggedToken(postag, string, offset) =>
      string + "/" + postag
    }.mkString(" ")
}

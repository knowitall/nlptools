package edu.washington.cs.knowitall
package tool
package postag

import common.main.LineProcessor

import scala.collection.JavaConversions._

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.tagger.maxent.MaxentTagger;

class StanfordPosTagger(
  val model: String = "edu/stanford/nlp/models/pos-tagger/wsj3t0-18-left3words/left3words-distsim-wsj-0-18.tagger",
  tokenizer: tokenize.Tokenizer = new tokenize.StanfordTokenizer()) 
extends PosTagger(tokenizer) {

  val tagger = new MaxentTagger(model)

  override def postag(tokens: Array[String]) =
    tagger.tagSentence(
      tokens.map { token =>
        val corelabel = new CoreLabel(); 
        corelabel.setWord(token); 
        corelabel 
      }.toList
    ).map(_.tag()).toArray
}

object StanfordPosTagger extends LineProcessor {
  val tagger = new StanfordPosTagger()
  override def process(line: String) = tagger.postag(line).map{case (a,b) => a + "/" + b}.mkString(" ")
}


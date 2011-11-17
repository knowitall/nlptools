package edu.washington.cs.knowitall
package tool
package parse

import stem.MorphaStemmer
import tokenize.OpenNlpTokenizer
import postag.OpenNlpPosTagger

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.parser.maltparser.MaltParserInterface
import graph._
import opennlp.tools.postag.POSTagger
import scala.collection.JavaConversions._
import edu.stanford.nlp.ling.CoreLabel

object MaltParser extends DependencyParserMain {
  var model = "engmalt.linear"

  override def init(args: Array[String]) {
    val index = args.indexOf("-m")
    if (index >= 0) {
      model = args(index + 1)
    }
  }

  lazy val parser = new MaltParser(model, null);
}

class MaltParser(modelname: String = "engmalt.linear", logfile: String = null) extends BaseStanfordParser {
  val parser = new MaltParserInterface(modelname, logfile)
  val tokenizer = new OpenNlpTokenizer
  val tagger = new OpenNlpPosTagger
  val stemmer = MorphaStemmer.instance
  
  private def depHelper(sentence: String, post: Boolean) = {
    val tokens = tokenizer.tokenize(sentence)
    val lemmas = tokens.map(stemmer.stem(_))
    val pos = tagger.postag(tokens)
    
    val labels = ((tokens zip lemmas) zip pos).map{ case ((t, l), p) => val cl = new CoreLabel(); cl.setWord(t); cl.setTag(p); cl.setLemma(l); cl }.toList
    val nodes = labels.view.zipWithIndex.map {
      case (tw, i) => (i, new DependencyNode(tw.word, tw.tag, i)) 
    }.toMap
    
    val gs = parser.parseToGrammaticalStructure(labels)
    
    (nodes, post match {
      case true => convertDependencies(nodes, gs.typedDependenciesCCprocessed)
      case false => convertDependencies(nodes, gs.typedDependencies)
    })
  }
  
  override def dependencies(sentence: String, post: Boolean) = {
    depHelper(sentence, post)._2
  }
  
  override def dependencyGraph(string: String, post: Boolean): DependencyGraph = {
    val (nodes, deps) = depHelper(string, post)
    new DependencyGraph(string, nodes.toArray.sortBy(_._1).map(_._2), deps)
  }
}

package edu.washington.cs.knowitall
package tool
package parse

import scala.collection.JavaConversions._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.parser.maltparser.MaltParserInterface
import graph.DependencyGraph
import graph.DependencyNode
import postag.OpenNlpPosTagger
import stem.MorphaStemmer
import tokenize.OpenNlpTokenizer
import tool.parse.BaseStanfordParser.CollapseType
import tool.postag.PostaggedToken

/** MaltParser is much faster than the StanfordParser but has a lower F-score.
  * It includes wrapper code so that it can still use the Stanford postprocessing.
  */
object MaltParser extends DependencyParserMain {
  var model = "engmalt.linear-1.7"

  override def init(args: Array[String]) {
    val index = args.indexOf("-m")
    if (index >= 0) {
      model = args(index + 1)
    }
  }

  lazy val parser = new MaltParser(model, null);
}

class MaltParser(modelname: String = "engmalt.linear-1.7", logfile: String = null) extends BaseStanfordParser {
  val parser = new MaltParserInterface(modelname, logfile)
  val tagger = new OpenNlpPosTagger
  val stemmer = MorphaStemmer.instance

  private def depHelper(sentence: String, collapser: CollapseType) = {
    val tokens = tagger.postag(sentence)
    val lemmas = tokens.map(_.string).map(stemmer.stem(_))

    val labels = (tokens zip lemmas).map { case (PostaggedToken(postag, string, offset), lemma) =>
      val cl = new CoreLabel(); cl.setWord(string); cl.setTag(postag); cl.setLemma(lemma); cl.setBeginPosition(offset); cl
    }.toList

    val nodes = labels.view.zipWithIndex.map {
      case (tw, i) => (i, new DependencyNode(tw.word, tw.tag, i, tw.beginPosition))
    }.toMap

    val gs = parser.parseToGrammaticalStructure(labels)

    (nodes, convertDependencies(nodes, collapser.collapse(gs)))
  }

  override def dependencies(sentence: String, collapse: CollapseType) = {
    depHelper(sentence, collapse)._2
  }

  override def dependencyGraph(string: String, collapse: CollapseType): DependencyGraph = {
    val (nodes, deps) = depHelper(string, collapse)
    new DependencyGraph(string, nodes.toSeq.sortBy(_._1).map(_._2), deps)
  }
}

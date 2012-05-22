package edu.washington.cs.knowitall
package tool
package parse

import scala.collection.JavaConversions._
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.PennTreebankLanguagePack
import graph.Dependency
import graph.DependencyGraph
import graph.DependencyNode
import edu.stanford.nlp.trees.GrammaticalStructure
import tool.parse.BaseStanfordParser._

/*
 * Subclasses of BaseStanfordParser must perform an optional post-processing step that applies
 * Stanford's CC-compressed algorithm on the graph. */
abstract class BaseStanfordParser extends DependencyParser {
  override def dependencies(string: String): Iterable[Dependency] = dependencies(string, CCCompressed)
  def dependencies(string: String, collapse: CollapseType): Iterable[Dependency]
  
  override def dependencyGraph(string: String) = dependencyGraph(string, CCCompressed)
  def dependencyGraph(string: String, collapse: CollapseType): DependencyGraph
  
  def convertDependency(nodes: Map[Int, DependencyNode], dep: edu.stanford.nlp.trees.TypedDependency) = {
    new Dependency(nodes(dep.gov.index - 1), nodes(dep.dep.index - 1), dep.reln.toString)
  }
  def convertDependencies(nodes: Map[Int, DependencyNode], dep: Iterable[edu.stanford.nlp.trees.TypedDependency]) = {
    // filter out the dependency from the root
    dep.filter(_.gov.index > 0).map(d => convertDependency(nodes, d))
  }
}

object BaseStanfordParser {
  sealed abstract class CollapseType {
    def collapse(gsf: GrammaticalStructure): Iterable[edu.stanford.nlp.trees.TypedDependency]
  }
  case object None extends CollapseType {
    override def collapse(gsf: GrammaticalStructure) = gsf.typedDependencies(false)
  }
  case object CCCompressed extends CollapseType {
    override def collapse(gsf: GrammaticalStructure) = gsf.typedDependenciesCCprocessed(false)
  }
  case object CollapsedTree extends CollapseType {
    override def collapse(gsf: GrammaticalStructure) = gsf.typedDependenciesCollapsedTree()
  }
  case object Collapsed extends CollapseType {
    override def collapse(gsf: GrammaticalStructure) = gsf.typedDependenciesCollapsed(false)
  }
}

object StanfordParser extends DependencyParserMain {
  lazy val parser = new StanfordParser
}


class StanfordParser(lp : LexicalizedParser) extends BaseStanfordParser with ConstituencyParser {
  def this() = this(LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz"))
  private val tlp = new PennTreebankLanguagePack();
  private val gsf = tlp.grammaticalStructureFactory();
  
  private def depHelper(string: String, collapser: CollapseType): (Map[Int, DependencyNode], Iterable[Dependency]) = {
    val tree = lp.apply(string)
    val nodes = tree.taggedYield().view.zipWithIndex.map {
      case (tw, i) => (i, new DependencyNode(tw.word, tw.tag, i)) 
    }.toMap
    
    (nodes, convertDependencies(nodes, collapser.collapse(gsf.newGrammaticalStructure(tree))))
  }

  override def dependencies(string: String, collapse: CollapseType): Iterable[Dependency] = 
    depHelper(string, collapse)._2
  
  override def dependencyGraph(string: String, collapse: CollapseType): DependencyGraph = {
    val (nodes, deps) = depHelper(string, collapse)
    new DependencyGraph(string, nodes.toList.sortBy(_._1).map(_._2), deps)
  }
  
  override def parse(string: String) = {
    var index = 0
    def convertTree(tree: edu.stanford.nlp.trees.Tree): ParseTree = {
      val curindex = index
      index += 1
      val children = tree.children.map(child => convertTree(child))
      if (tree.isPhrasal)
        new ParseTreePhrase(tree.value, curindex, children)
      else if (tree.isPreTerminal)
        new ParseTreePostag(tree.value, curindex, children)
      else
        new ParseTreeToken(tree.value, curindex, children)
    }
    convertTree(lp.apply(string))
  }
}

object StanfordConstituencyParser 
extends ConstituencyParserMain {
  lazy val parser = new StanfordParser();
}

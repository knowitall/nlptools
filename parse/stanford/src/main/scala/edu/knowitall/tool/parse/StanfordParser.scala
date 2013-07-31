package edu.knowitall
package tool
package parse

import scala.Array.canBuildFrom
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.collectionAsScalaIterable
import edu.knowitall.tool.postag.Postagger
import edu.knowitall.tool.postag.StanfordPostagger
import edu.stanford.nlp.ling.TaggedWord
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.GrammaticalStructure
import edu.stanford.nlp.trees.PennTreebankLanguagePack
import edu.stanford.nlp.trees.Tree
import graph.Dependency
import graph.DependencyGraph
import graph.DependencyNode
import postag.PostaggedToken
import tokenize.Token
import edu.knowitall.tool.parse.BaseStanfordParser.CollapseType

/*
 * Subclasses of BaseStanfordParser must perform an optional post-processing step that applies
 * Stanford's CC-compressed algorithm on the graph. */
abstract class BaseStanfordParser extends DependencyParser {
  
  @deprecated("Use dependencyGraph(string, collapse).dependencies", "2.4.3")
  def dependencies(string: String, collapse: CollapseType): Iterable[Dependency] = dependencyGraph(string, collapse).dependencies

  @deprecated("Use dependencyGraph(string).dependencies", "2.4.3")
  override def dependencies(string: String): Iterable[Dependency] = dependencies(string, BaseStanfordParser.None)

  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    dependencyGraphPostagged(tokens, BaseStanfordParser.None)
  }
  
  def dependencyGraphPostagged(tokens: Seq[PostaggedToken], collapse: CollapseType): DependencyGraph
  
  def dependencyGraph(string: String, collapse: CollapseType) = {
    val postaggedTokens = postagger.postag(string)
    dependencyGraphPostagged(postaggedTokens, collapse)
  }
  
  def dependencyGraphTokenized(tokens: Seq[Token], collapse: CollapseType) = {
    val postaggedTokens = postagger.postagTokens(tokens)
    dependencyGraphPostagged(postaggedTokens, collapse)
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

object StanfordParserMain extends DependencyParserMain {
  lazy val dependencyParser = new StanfordParser
}

class StanfordParser(lp: LexicalizedParser, val postagger: Postagger) extends BaseStanfordParser with ConstituencyParser {
  def this(postagger: Postagger = new StanfordPostagger()) = this(LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz"), postagger)

  override def dependencies(string: String, collapse: CollapseType): Iterable[Dependency] = {
    val tokens = postagger.postag(string)
    StanfordParser.dependencyHelper(lp.parse(postagToStanfordRepr(tokens)), collapse)._2
  }

  private def postagToStanfordRepr(tokens: Seq[PostaggedToken]): java.util.List[TaggedWord] = {
    val words = new java.util.ArrayList[TaggedWord](tokens.size)

    tokens.foreach { token =>
      val w = new TaggedWord(token.string, token.postag)
      w.setBeginPosition(token.offsets.start)
      w.setEndPosition(token.offsets.end)
      words.add(w)
    }

    words
  }
   
  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken], collapse: CollapseType) = {
    val (nodes, deps) = StanfordParser.dependencyHelper(lp.parse(postagToStanfordRepr(tokens)), collapse)
    new DependencyGraph(nodes.toList.sortBy(_.indices), deps)
  }

  override def parse(string: String) = {
    val tokens = postagger.postag(string)
    StanfordParser.convertTree(lp.parse(postagToStanfordRepr(tokens)))
  }
}

object StanfordParser {
  private val tlp = new PennTreebankLanguagePack()
  private val gsf = tlp.grammaticalStructureFactory()

  def convertTree(tree: edu.stanford.nlp.trees.Tree): ParseTree = {
    var index = 0
    def rec(tree: edu.stanford.nlp.trees.Tree): ParseTree = {
      val curindex = index
      index += 1
      val children = tree.children.map(child => rec(child))
      if (tree.isPhrasal)
        new ParseTreePhrase(tree.value, curindex, children)
      else if (tree.isPreTerminal)
        new ParseTreePostag(tree.value, curindex, children)
      else
        new ParseTreeToken(tree.value, curindex, children)
    }

    rec(tree)
  }

  def dependencyHelper(tree: Tree, collapser: CollapseType): (Iterable[DependencyNode], Iterable[Dependency]) = {
    val nodes = tree.taggedYield().view.zipWithIndex.map {
      case (tw, i) => new DependencyNode(tw.word, tw.tag, i, tw.beginPosition)
    }

    (nodes, convertDependencies(nodes, collapser.collapse(gsf.newGrammaticalStructure(tree))))
  }

  def convertDependency(nodes: Map[Int, DependencyNode], dep: edu.stanford.nlp.trees.TypedDependency) = {
    new Dependency(nodes(dep.gov.index - 1), nodes(dep.dep.index - 1), dep.reln.toString)
  }

  def convertDependencies(nodes: Iterable[DependencyNode], dep: Iterable[edu.stanford.nlp.trees.TypedDependency]) = {
    // filter out the dependency from the root
    dep.filter(_.gov.index > 0).map(d => convertDependency(nodes.map(node => node.indices.head -> node).toMap, d))
  }
}

object StanfordConstituencyParser
  extends ConstituencyParserMain {
  lazy val constituencyParser = new StanfordParser();
}

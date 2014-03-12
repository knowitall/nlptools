package edu.knowitall
package tool
package parse

import scala.Array.canBuildFrom
import scala.collection.JavaConversions.asScalaBuffer
import edu.knowitall.tool.parse.BaseStanfordParser.CollapseType
import edu.knowitall.tool.postag.Postagger
import edu.knowitall.tool.postag.StanfordPostagger
import edu.stanford.nlp.ling.TaggedWord
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.PennTreebankLanguagePack
import edu.stanford.nlp.trees.Tree
import graph.Dependency
import graph.DependencyGraph
import graph.DependencyNode
import postag.PostaggedToken
import edu.knowitall.tool.tokenize.Token

class StanfordParser(lp: LexicalizedParser, val postagger: Postagger) extends BaseStanfordParser with ConstituencyParser {
  def this(postagger: Postagger = new StanfordPostagger()) =
    this(LexicalizedParser.loadModel(StanfordParser.pcfgModelPath), postagger)

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
    DependencyGraph.create(deps)
  }

  /**
    * Create a graph of the dependencies from Tokens without postagging.
    */
  def dependencyGraphWithoutPostags(tokens: Seq[Token]): DependencyGraph = {
    val postaggedTokens = tokens.map { t => PostaggedToken(t, null) }
    dependencyGraphPostagged(postaggedTokens)
  }

  /**
    * Create a graph of the dependencies from Tokens without postagging.
    */
  def dependencyGraphWithoutPostags(text: String): (Seq[Token], DependencyGraph) = {
    val tokens = postagger.tokenizer(text)
    val graph = this.dependencyGraphWithoutPostags(tokens)
    (tokens, graph)
  }

  override def parse(string: String) = {
    val tokens = postagger.postag(string)
    StanfordParser.convertTree(lp.parse(postagToStanfordRepr(tokens)))
  }
}

object StanfordParser {
  val pcfgModelPath = "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz"
  val rnnModelPath = "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz"

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
      case (tw, i) => new DependencyNode(i, tw.word)
    }

    (nodes, convertDependencies(nodes, collapser.collapse(gsf.newGrammaticalStructure(tree))))
  }

  def convertDependency(nodes: Map[Int, DependencyNode], dep: edu.stanford.nlp.trees.TypedDependency) = {
    new Dependency(nodes(dep.gov.index - 1), nodes(dep.dep.index - 1), dep.reln.toString)
  }

  def convertDependencies(nodes: Iterable[DependencyNode], dep: Iterable[edu.stanford.nlp.trees.TypedDependency]) = {
    // filter out the dependency from the root
    dep.filter(_.gov.index > 0).map(d => convertDependency(nodes.map(node => node.id -> node).toMap, d))
  }
}

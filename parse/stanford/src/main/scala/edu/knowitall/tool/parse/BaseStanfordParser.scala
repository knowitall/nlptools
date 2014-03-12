package edu.knowitall
package tool
package parse

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.collectionAsScalaIterable
import edu.knowitall.tool.parse.BaseStanfordParser.CollapseType
import edu.stanford.nlp.trees.GrammaticalStructure
import graph.DependencyGraph
import postag.PostaggedToken
import tokenize.Token

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

/*
 * Subclasses of BaseStanfordParser must perform an optional post-processing step that applies
 * Stanford's CC-compressed algorithm on the graph. */
abstract class BaseStanfordParser extends DependencyParser {

  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    dependencyGraphPostagged(tokens, BaseStanfordParser.None)
  }

  def dependencyGraphPostagged(tokens: Seq[PostaggedToken], collapse: CollapseType): DependencyGraph

  def dependencyGraph(string: String, collapse: CollapseType) = {
    val postaggedTokens = postagger.postag(string)
    dependencyGraphPostagged(postaggedTokens, collapse)
  }

  def dependencyGraphTokenized(tokens: Seq[Token], collapse: CollapseType) = {
    val postaggedTokens = postagger.postagTokenized(tokens)
    dependencyGraphPostagged(postaggedTokens, collapse)
  }
}

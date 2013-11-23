package edu.knowitall
package tool
package parse
package graph

import edu.knowitall.collection.immutable.graph.Graph
import edu.knowitall.collection.immutable.graph.Graph._
import scala.collection.immutable.SortedSet
import edu.knowitall.collection.immutable.Interval
import tool.stem.{ Stemmer, IdentityStemmer }
import tool.postag.PostaggedToken
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.stem.Lemmatized

/** A representation for a node in the graph of dependencies.  A node
  * represents one or more adjacent tokens in the source sentence.
  */
case class TokenDependencyNode(val id: Int, val token: Lemmatized[PostaggedToken]) {
  def string = token.token.string
  def postag = token.token.postag
  def lemma = token.lemma

  // extend Object
  override def toString() = s"$string-$id"
}

object TokenDependencyNode {
  def from(tokens: Seq[Lemmatized[PostaggedToken]])(node: DependencyNode) = 
    TokenDependencyNode(node.id, tokens(node.id))
}

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

/** A representation for a node in the graph of dependencies.  A node
  * represents one or more adjacent tokens in the source sentence.
  */
case class JoinedDependencyNode(val ids: Seq[Int], val strings: Seq[String]) {
  require(!ids.isEmpty)
  require(!strings.isEmpty)

  def string = strings.mkString(" ")
  
  def span = Interval.closed(ids.min, ids.max)

  // extend Object
  override def toString() = s"${strings.mkString(" ")}-${ids.mkString(",")}"
}

object JoinedDependencyNode {
  def from(node: DependencyNode) = JoinedDependencyNode(Seq(node.id), Seq(node.string))

  /**
   * Merge nodes that correspond to adjacent tokens.
   *
   * @throws  IllegalArgumentException  there is no superior of the set
   * @return  the superior node of the set
   */
  implicit def directedMerge(graph: Graph[JoinedDependencyNode])(nodes: Traversable[JoinedDependencyNode]) = {
    if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
    val sorted = nodes.toList.sortBy(_.span)
    val strings = sorted.map(_.string)

    // ensure the nodes are adjacent in the source sentence
    // or at least that the spans are
    val spans = sorted.map(_.span)
    if (!(Interval.span(spans) forall (point => spans.exists(span => span contains point)))) {
      throw new IllegalArgumentException("A set of non-adjacent intervals cannot be merged: " + nodes.mkString(", "))
    }

    new JoinedDependencyNode(sorted.flatMap(_.ids).sorted, strings)
  }
}
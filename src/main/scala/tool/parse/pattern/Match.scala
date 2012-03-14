package edu.washington.cs.knowitall
package tool
package parse
package pattern

import tool.parse.graph._

/** A representation of a match of a pattern in a graph. */
class Match[T](
  /** the pattern that was applied */
  val pattern: Pattern[T], 
  /** the matched path through the graph */
  val bipath: Bipath[T], 
  /** the pattern groups in the match */
  val nodeGroups: Map[String, List[Match.NodeGroup[T]]],
  val edgeGroups: Map[String, List[Match.EdgeGroup[T]]]
) {
  // extend Object
  override def toString = bipath.toString + ": " + nodeGroups.toString + " and " + edgeGroups.toString

  def groups: Map[String, List[Match.Group]] = nodeGroups ++ edgeGroups

  def nodes: Iterable[T] = bipath.nodes
  def edges: Iterable[Graph.Edge[T]] = bipath.edges
}

object Match {
  sealed abstract class Group(val text: String)
  case class NodeGroup[T](node: T, matchText: String) extends Group(matchText)
  case class EdgeGroup[T](dedge: DirectedEdge[T], matchText: String) extends Group(matchText)
}
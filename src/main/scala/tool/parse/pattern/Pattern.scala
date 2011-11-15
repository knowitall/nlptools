package edu.washington.cs.knowitall
package tool
package parse
package pattern

import common._

import graph._
import collection._


/**
  * Represents a pattern with which graphs can be searched.  
  * A pattern will start and end with a node matcher, and every
  * matcher (necessarily) alternates between a NodeMatcher and
  * and EdgeMatcher.
  */
class Pattern[V](val matchers: List[Matcher[V]]) extends Function[Graph[V], List[Match[V]]] {
  // ensure that the matchers alternate
  matchers.view.zipWithIndex.foreach { case(m, i) => 
    (m, (i%2)) match {
      case (m: NodeMatcher[_], 0) =>
      case (m: EdgeMatcher[_], 1) =>
      case _ => throw new IllegalArgumentException("matchers must start with a node matcher and alternate")
    }
  }

  def this(edgeMatchers: List[EdgeMatcher[V]], nodeMatchers: List[NodeMatcher[V]]) = {
    this(pimp.Iterables.interleave(nodeMatchers, edgeMatchers).toList)
  }
  
  def apply(graph: Graph[V]): List[Match[V]] = {
    graph.nodes.view.toList.flatMap(apply(graph, _).toList)
  }

  def apply(graph: Graph[V], vertex: V): Option[Match[V]] = {
    def rec(matchers: List[Matcher[V]], 
      vertex: V, 
      edges: List[DirectedEdge[V]],
      groups: List[(String, V)]): Option[Match[V]] = matchers match {

      case (m: CaptureNodeMatcher[_]) :: xs =>
        if (m.matches(vertex)) rec(xs, vertex, edges, (m.alias, vertex) :: groups)
        else None
      case (m: NodeMatcher[_]) :: xs => 
        if (m.matches(vertex)) rec(xs, vertex, edges, groups)
        else None
      case (m: EdgeMatcher[_]) :: xs => 
        // only consider edges that have not been used
        val uniqueEdges = graph.dedges(vertex)--edges.flatMap(e => List(e, e.flip))
        // search for an edge that matches
        uniqueEdges.find(m.matches(_)).flatMap { dedge =>
          // we found one, so recurse
          rec(xs, dedge.end, dedge :: edges, groups)
        }
      case _ => Some(new Match(this, new Bipath(edges.reverse), groups.toMap))
    }

    rec(this.matchers, vertex, List(), List())
  }
  
  def replaceMatcherAt(index: Int, replacement: NodeMatcher[V]) = 
    new Pattern(
      matchers.view.zipWithIndex.map {
        case (matcher, i) => if (i == index) replacement else matcher 
      }.toList)
  
  override def toString = {
    matchers.view.map(_.toString).mkString(" ")
  }
}

class Match[V](val pattern: Pattern[V], 
  val bipath: Bipath[V], 
  val groups: Map[String, V]) {
  override def toString = bipath.toString + ": " + groups.toString
}

abstract class Matcher[V]

/**
  * Trait to match dependency graph edges. 
  */
trait EdgeMatcher[V] extends Matcher[V] {
  def matches(edge: DirectedEdge[V]): Boolean
}

/**
  * Trait to match dependency graph nodes. 
  */
trait NodeMatcher[V] extends Matcher[V] {
  def matches(node: V): Boolean
}

class TrivialNodeMatcher[V] extends NodeMatcher[V] {
  override def matches(edge: V) = true
  override def toString = ".*"
}

/**
  * Trait that captures the contents of a node if it's matched.
  * @param  alias  the name of the captured node
  * @param  matcher  the matcher to apply
  */
class CaptureNodeMatcher[V](val alias: String, val matcher: NodeMatcher[V]) 
extends NodeMatcher[V] {
  /**
    * Convenience constructor that uses the TrivialNodeMatcher.
    */
  def this(alias: String) = this(alias, new TrivialNodeMatcher)

  override def matches(node: V) = matcher.matches(node)
  override def toString = "{" + alias + "}"
}

package edu.washington.cs.knowitall
package tool
package parse
package pattern

import common._

import graph._
import collection._

import scala.util.matching.Regex

/**
  * Represents a pattern with which graphs can be searched.  
  * A pattern will start and end with a node matcher, and every
  * matcher (necessarily) alternates between a NodeMatcher and
  * and EdgeMatcher.
  */
class Pattern[T](
  /** a list of matchers, alternating between `NodeMatcher`s and `EdgeMatcher`s. */
  val matchers: List[Matcher[T]]
) extends Function[Graph[T], List[Match[T]]] {

  // ensure that the matchers alternate
  matchers.view.zipWithIndex.foreach { case(m, i) => 
    (m, (i%2)) match {
      case (m: NodeMatcher[_], 0) =>
      case (m: EdgeMatcher[_], 1) =>
      case _ => throw new IllegalArgumentException("matchers must start with a node matcher and alternate")
    }
  }

  def this(edgeMatchers: List[EdgeMatcher[T]], nodeMatchers: List[NodeMatcher[T]]) = {
    this(enrich.Iterables.interleave(nodeMatchers, edgeMatchers).toList)
  }

  // extend Object
  override def toString = {
    matchers.view.map(_.toString).mkString(" ") }
  
  /** Find all matches of this pattern in the graph. */
  def apply(graph: Graph[T]): List[Match[T]] = {
    graph.vertices.toList.flatMap(apply(graph, _).toList)
  }

  /** Find all matches of this pattern in the graph starting at `vertex`. */
  def apply(graph: Graph[T], vertex: T): List[Match[T]] = {
    def rec(matchers: List[Matcher[T]], 
      vertex: T, 
      edges: List[DirectedEdge[T]],
      nodeGroups: List[(String, T)],
      edgeGroups: List[(String, DirectedEdge[T])]): List[Match[T]] = matchers match {

      case (m: CaptureNodeMatcher[_]) :: xs =>
        if (m.matches(vertex)) rec(xs, vertex, edges, (m.alias, vertex) :: nodeGroups, edgeGroups)
        else List()
      case (m: NodeMatcher[_]) :: xs => 
        if (m.matches(vertex)) rec(xs, vertex, edges, nodeGroups, edgeGroups)
        else List()
      case (m: EdgeMatcher[_]) :: xs => 
        // only consider edges that have not been used
        val uniqueEdges = graph.dedges(vertex)--edges.flatMap(e => List(e, e.flip))
        // search for an edge that matches
        uniqueEdges.filter(m.matches(_)).flatMap { dedge =>
          val groups = m match {
            case m: CaptureEdgeMatcher[_] => (m.alias, dedge) :: edgeGroups
            case _ => edgeGroups
          }
          // we found one, so recurse
          rec(xs, dedge.end, dedge :: edges, nodeGroups, groups)
        }(scala.collection.breakOut)
      case _ => List(new Match(this, new Bipath(edges.reverse), nodeGroups.toMap, edgeGroups.toMap))
    }

    rec(this.matchers, vertex, List(), List(), List())
  }
  
  /** A list of just the edge matchers, in order. */
  def edgeMatchers = matchers.collect { case m: EdgeMatcher[_] => m }
  /** A list of just the node matchers, in order. */
  def nodeMatchers = matchers.collect { case m: NodeMatcher[_] => m }

  def replaceMatcherAt(replacements: List[(Int, NodeMatcher[T])]) = 
    new Pattern(
      matchers.view.zipWithIndex.map {
        case (matcher, i) => replacements.find(_._1 == i).map(_._2) getOrElse matcher
      }.toList)
  
  def replaceMatcherAt(index: Int, replacement: NodeMatcher[T]) = 
    new Pattern(
      matchers.view.zipWithIndex.map {
        case (matcher, i) => if (i == index) replacement else matcher 
      }.toList)
}

/** A representation of a match of a pattern in a graph. */
class Match[T](
  /** the pattern that was applied */
  val pattern: Pattern[T], 
  /** the matched path through the graph */
  val bipath: Bipath[T], 
  /** the pattern groups in the match */
  val nodeGroups: Map[String, T],
  val edgeGroups: Map[String, DirectedEdge[T]]
) {
  // extend Object
  override def toString = bipath.toString + ": " + nodeGroups.toString + " and " + edgeGroups.toString
}

/**
  * Abstract superclass for all matchers. */
abstract class Matcher[T]

/**
  * Trait to match dependency graph edges. 
  */
trait EdgeMatcher[T] extends Matcher[T] {
  def matches(edge: DirectedEdge[T]): Boolean
  def canMatch(edge: Graph.Edge[T]): Boolean = this.matches(new UpEdge(edge)) || this.matches(new DownEdge(edge))
  def flip: EdgeMatcher[T]
}

class TrivialEdgeMatcher[T] extends EdgeMatcher[T] {
  def matches(edge: DirectedEdge[T]) = true
  def flip = this
}

class CaptureEdgeMatcher[T](val alias: String, val matcher: EdgeMatcher[T]) extends EdgeMatcher[T] {
  override def matches(edge: DirectedEdge[T]) = matcher.matches(edge)
  override def flip = new CaptureEdgeMatcher(alias, matcher.flip)
  
  // extend Object
  override def toString = "{" + alias + "}"
  def canEqual(that: Any) = that.isInstanceOf[CaptureEdgeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: CaptureNodeMatcher[_] => (that canEqual this) && this.alias == that.alias && this.matcher == that.matcher
    case _ => false
  }
  override def hashCode = alias.hashCode + 39*matcher.hashCode
}

/**
  * Trait to match dependency graph nodes. 
  */
trait NodeMatcher[T] extends Matcher[T] {
  def matches(node: T): Boolean
}

/**
  * Always match any node. */
class TrivialNodeMatcher[T] extends NodeMatcher[T] {
  override def matches(edge: T) = true

  // extend Object
  override def toString = ".*"
  def canEqual(that: Any) = that.isInstanceOf[TrivialNodeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: TrivialNodeMatcher[_] => that canEqual this
    case _ => false
  }
  override def hashCode = toString.hashCode
}

/**
  * Trait that captures the contents of a node if it's matched.
  * @param  alias  the name of the captured node
  * @param  matcher  the matcher to apply
  */
class CaptureNodeMatcher[T](val alias: String, val matcher: NodeMatcher[T]) 
extends NodeMatcher[T] {
  /**
    * Convenience constructor that uses the TrivialNodeMatcher.
    */
  def this(alias: String) = this(alias, new TrivialNodeMatcher)

  override def matches(node: T) = matcher.matches(node)
  
  // extend Object
  override def toString = "{" + alias + "}"
  def canEqual(that: Any) = that.isInstanceOf[CaptureNodeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: CaptureNodeMatcher[_] => (that canEqual this) && this.alias == that.alias && this.matcher == that.matcher
    case _ => false
  }
  override def hashCode = alias.hashCode + 39*matcher.hashCode
}

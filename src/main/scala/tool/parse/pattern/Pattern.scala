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
  
  require(matchers != null)

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
  def canEqual(that: Any) = that.isInstanceOf[Pattern[_]]
  override def equals(that: Any) = that match {
    case that: Pattern[_] => (that canEqual this) && this.matchers == that.matchers
    case _ => false
  }
  override def hashCode = this.matchers.hashCode
  
  /** Find all matches of this pattern in the graph. */
  def apply(graph: Graph[T]): List[Match[T]] = {
    graph.vertices.toList.flatMap(apply(graph, _).toList)
  }

  /** Find all matches of this pattern in the graph starting at `vertex`. */
  def apply(graph: Graph[T], vertex: T): List[Match[T]] = {
    def rec(matchers: List[Matcher[T]], 
      vertex: T, 
      edges: List[DirectedEdge[T]],
      nodeGroups: List[(String, Match.NodeGroup[T])],
      edgeGroups: List[(String, Match.EdgeGroup[T])]): List[Match[T]] = matchers match {

      case (m: CaptureNodeMatcher[_]) :: xs =>
        m.matchText(vertex).map(text => rec(xs, vertex, edges, (m.alias, Match.NodeGroup(vertex, text)) :: nodeGroups, edgeGroups)).getOrElse(List())
      case (m: NodeMatcher[_]) :: xs if m.matches(vertex) => 
        if (m.matches(vertex)) rec(xs, vertex, edges, nodeGroups, edgeGroups)
        else List()
      case (m: EdgeMatcher[_]) :: xs => 
        // only consider edges that have not been used
        val uniqueEdges = graph.dedges(vertex)--edges.flatMap(e => List(e, e.flip))
        // search for an edge that matches
        uniqueEdges.flatMap { edge => 
          m.matchText(edge).map(text => (edge, text))
        }.flatMap { case (dedge, matchText) =>
          val groups = m match {
            case m: CaptureEdgeMatcher[_] => (m.alias, Match.EdgeGroup(dedge, matchText)) :: edgeGroups
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
  val nodeGroups: Map[String, Match.NodeGroup[T]],
  val edgeGroups: Map[String, Match.EdgeGroup[T]]
) {
  // extend Object
  override def toString = bipath.toString + ": " + nodeGroups.toString + " and " + edgeGroups.toString

  def groups: Map[String, Match.Group] = nodeGroups ++ edgeGroups

  def nodes: Iterable[T] = bipath.nodes
  def edges: Iterable[Graph.Edge[T]] = bipath.edges
}

object Match {
  sealed abstract class Group(val text: String)
  case class NodeGroup[T](node: T, matchText: String) extends Group(matchText)
  case class EdgeGroup[T](dedge: DirectedEdge[T], matchText: String) extends Group(matchText)
}

/**
  * Abstract superclass for all matchers. */
abstract class Matcher[T]

/**
  * Trait to match dependency graph edges. 
  */
trait EdgeMatcher[T] extends Matcher[T] {
  def apply(edge: DirectedEdge[T]) = this.matchText(edge)

  def matches(edge: DirectedEdge[T]) = this.matchText(edge).isDefined
  def matchText(edge: DirectedEdge[T]): Option[String]

  def canMatch(edge: Graph.Edge[T]): Boolean = this.matches(new UpEdge(edge)) || this.matches(new DownEdge(edge))
  def flip: EdgeMatcher[T]
}

class DirectedEdgeMatcher[T](val direction: Direction, val matcher: EdgeMatcher[T]) extends EdgeMatcher[T] {
  def matchText(edge: DirectedEdge[T]) = 
    if (edge.dir == direction) matcher.matchText(edge)
    else None

  def flip: EdgeMatcher[T] = new DirectedEdgeMatcher(direction.flip, matcher)
 
  /** symbolic representation used in serialization. */
  def symbol = direction match { 
    case Direction.Up => "<" 
    case Direction.Down => ">" 
  }

  // extend Object
  override def toString = symbol + matcher.toString + symbol
  def canEqual(that: Any) = that.isInstanceOf[DirectedEdgeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: DirectedEdgeMatcher[_] => (that canEqual this) && this.direction == that.direction && this.matcher == that.matcher
    case _ => false
  }
  override def hashCode = direction.hashCode + 39*matcher.hashCode
}

class TrivialEdgeMatcher[T] extends EdgeMatcher[T] {
  def matchText(edge: DirectedEdge[T]) = Some(edge.edge.label)
  def flip = this
}

class CaptureEdgeMatcher[T](val alias: String, val matcher: EdgeMatcher[T]) extends EdgeMatcher[T] {
  override def matchText(edge: DirectedEdge[T]) = matcher.matchText(edge)
  override def flip = new CaptureEdgeMatcher(alias, matcher.flip)
  
  // extend Object
  override def toString = matcher match {
    case _: TrivialEdgeMatcher[_] => "{"+alias+"}"
    case d: DirectedEdgeMatcher[_] => d.symbol+"{"+alias+":"+d.matcher.toString+"}"+d.symbol
    case m: EdgeMatcher[_] => "{"+alias+":"+m.toString+"}"
  }
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
  def apply(node: T) = this.matchText(node)

  def matches(node: T) = this.matchText(node).isDefined
  def matchText(node: T): Option[String]
}

/**
  * Always match any node. */
class TrivialNodeMatcher[T] extends NodeMatcher[T] {
  override def matchText(node: T) = Some(".*")

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

  override def matchText(node: T) = matcher.matchText(node)
  
  // extend Object
  override def toString = {
    "{"+(matcher match {
      case m: TrivialNodeMatcher[_] => alias
      case m => alias + ":" + m.toString
    })+"}"
  }
  def canEqual(that: Any) = that.isInstanceOf[CaptureNodeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: CaptureNodeMatcher[_] => (that canEqual this) && this.alias == that.alias && this.matcher == that.matcher
    case _ => false
  }
  override def hashCode = alias.hashCode + 39*matcher.hashCode
}

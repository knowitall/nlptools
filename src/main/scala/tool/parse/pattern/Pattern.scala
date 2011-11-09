package edu.washington.cs.knowitall
package tool
package parse
package pattern

import common._

import graph._
import collection._


/**
  * Represents a pattern with which parse trees can be searched.  
  * A pattern will start and end with a node matcher, and every
  * matcher (necessarily) alternates between a NodeMatcher and
  * and EdgeMatcher.
  */
class Pattern(val matchers: List[Matcher]) extends Function[DependencyGraph, List[Match]] {
  // ensure that the matchers alternate
  matchers.view.zipWithIndex.foreach { case(m, i) => 
    (m, (i%2)) match {
      case (m: NodeMatcher, 0) =>
      case (m: EdgeMatcher, 1) =>
      case _ => throw new IllegalArgumentException("matchers must start with a node matcher and alternate")
    }
  }

  def this(edgeMatchers: List[EdgeMatcher], nodeMatchers: List[NodeMatcher]) = {
    this(pimp.Iterables.interleave(nodeMatchers, edgeMatchers).toList)
  }

  /**
    * A more intuitive constructor that builds the pattern from a 
    * bidirectional path though the tree.
    */
  def this(bipath: Bipath) = this(
    bipath.path.map(dedge => new DefaultEdgeMatcher(dedge)),
    new DefaultNodeMatcher(bipath.path.head.start) :: bipath.path.map(dedge => new DefaultNodeMatcher(dedge.end))
  )
  
  def apply(graph: DependencyGraph): List[Match] = {
    graph.nodes.view.toList.flatMap(apply(graph, _).toList)
  }

  def apply(graph: DependencyGraph, vertex: DependencyNode): Option[Match] = {
    def rec(matchers: List[Matcher], 
      vertex: DependencyNode, 
      edges: List[DirectedEdge],
      groups: List[(String, DependencyNode)]): Option[Match] = matchers match {

      case (m: CaptureNodeMatcher) :: xs =>
        if (m.matches(vertex)) rec(xs, vertex, edges, (m.alias, vertex) :: groups)
        else None
      case (m: NodeMatcher) :: xs => 
        if (m.matches(vertex)) rec(xs, vertex, edges, groups)
        else None
      case (m: EdgeMatcher) :: xs => 
        // search for an edge that matches
        graph.dedges(vertex).find(m.matches(_)).flatMap { dedge =>
          // we found one, so recurse
          rec(xs, dedge.end, dedge :: edges, groups)
        }
      case _ => Some(new Match(this, new Bipath(edges.reverse), groups.toMap))
    }

    rec(this.matchers, vertex, List(), List())
  }
  
  def replaceNodeMatcherAtIndex(index: Int, replacement: NodeMatcher) = 
    new Pattern(
      matchers.view.zipWithIndex.map {
        case (matcher, i) => if (i == index*2) replacement else matcher 
      }.toList)
  
  override def toString = {
    matchers.view.map(_.toString).mkString(" ")
  }
}

object Pattern {
  import scala.util.parsing.combinator._

  object PatternParser extends RegexParsers {
    def simpleNodeMatcher = """\w+""".r ^^ { s => new DefaultNodeMatcher(s) }
    def captureNodeMatcher = "{" ~> """\w+""".r <~ "}" ^^ { s => new CaptureNodeMatcher(s) }
    def nodeMatcher: Parser[NodeMatcher] = (captureNodeMatcher | simpleNodeMatcher) ^^ { s => s.asInstanceOf[NodeMatcher] }
    
    def upEdgeMatcher = "<" ~> """\w+""".r <~ "<" ^^ { s => new DefaultEdgeMatcher(s, Direction.Up) }
    def downEdgeMatcher = ">" ~> """\w+""".r <~ ">" ^^ { s => new DefaultEdgeMatcher(s, Direction.Down) }
    def edgeMatcher: Parser[EdgeMatcher] = (upEdgeMatcher | downEdgeMatcher) ^^ { s => s.asInstanceOf[EdgeMatcher] }
    
    // def chain = nodeMatcher ~! edgeMatcher ^^ { case n ~ e => List(n, e) }
    def chain: Parser[List[Matcher]] = nodeMatcher ~ edgeMatcher ~ chain ^^ { case n ~ e ~ ch => n :: e :: ch } | nodeMatcher ^^ { List(_) }
    // def expr = chain ~ nodeMatcher ^^ { case ch ~ n => ch ::: List(n)}
    
    def parse(s: String) = {
      parseAll(chain, s)
    }

    def apply(s: String): Pattern = {
      parse(s) match {
        case Success(matchers, _) => new Pattern(matchers)
        case e: NoSuccess =>
          System.err.println(e)
          throw new IllegalArgumentException("improper pattern syntax: " + s)
      }
    }
  }
  
  def deserialize(string: String): Pattern = {
    PatternParser(string)
  }
}

class Match(val pattern: Pattern, 
  val bipath: Bipath, 
  val groups: Map[String, DependencyNode]) {
  override def toString = bipath.toString + ": " + groups.toString
}

abstract class Matcher

/**
  * Trait to match dependency graph edges. 
  */
trait EdgeMatcher extends Matcher {
  def matches(edge: DirectedEdge): Boolean
}

class DefaultEdgeMatcher(val label: String, val dir: Direction) extends EdgeMatcher {
  def this(dedge: DirectedEdge) = this(dedge.edge.label, dedge.dir)
  
  override def matches(edge: DirectedEdge) =
    edge.dir == dir && edge.edge.label == label
    
  def symbol = dir match { case Direction.Up => "<" case Direction.Down => ">" }
  override def toString = symbol + label + symbol
}

/**
  * Trait to match dependency graph nodes. 
  */
trait NodeMatcher extends Matcher {
  def matches(edge: DependencyNode): Boolean
}

class TrivialNodeMatcher extends NodeMatcher {
  override def matches(edge: DependencyNode) = true
  override def toString = ".*"
}

class DefaultNodeMatcher(val label: String) extends NodeMatcher {
  def this(node: DependencyNode) = this(node.text)
  
  override def matches(node: DependencyNode) = node.text == label
  override def toString = label
}

/**
  * Trait that captures the contents of a node if it's matched.
  * @param  alias  the name of the captured node
  * @param  matcher  the matcher to apply
  */
class CaptureNodeMatcher(val alias: String, val matcher: NodeMatcher) 
extends NodeMatcher {
  /**
    * Convenience constructor that uses the TrivialNodeMatcher.
    */
  def this(alias: String) = this(alias, new TrivialNodeMatcher)

  override def matches(node: DependencyNode) = matcher.matches(node)
  override def toString = "{" + alias + "}"
}

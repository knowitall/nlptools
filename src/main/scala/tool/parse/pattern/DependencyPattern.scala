package edu.washington.cs.knowitall
package tool
package parse
package pattern

import graph._

/**
  * A pattern over a graph of dependencies. */
class DependencyPattern(matchers: List[Matcher[DependencyNode]]) extends Pattern[DependencyNode](matchers) {
  def depEdgeMatchers = matchers.collect { case m: DependencyEdgeMatcher => m }
  def depNodeMatchers = matchers.collect { case m: DependencyNodeMatcher => m }
}

object DependencyPattern {
  import scala.util.parsing.combinator._

  /**
    * A parser combinator for deserializing patterns over graphs of dependencies. */
  object Parser extends RegexParsers {
    def simpleNodeMatcher = """\w+""".r ^^ { s => new DependencyNodeMatcher(Some(s), None) with MatchLabel }
    def simpleCaptureNodeMatcher = "{" ~> """\w+""".r <~ "}" ^^ { s => new CaptureNodeMatcher[DependencyNode](s) }
    def postagCaptureNodeMatcher = "{" ~> """\w+""".r ~ ":" ~ """[^}\p{Space}]+""".r <~ "}" ^^ { case s~":"~postag => new CaptureNodeMatcher[DependencyNode](s, new DependencyNodeMatcher(None, Some(postag)) with MatchPostag) { override def toString = "{" + this.alias + ":" + this.matcher.asInstanceOf[DependencyNodeMatcher].postag.get + "}"} }
    def captureNodeMatcher[V]: Parser[CaptureNodeMatcher[DependencyNode]] = simpleCaptureNodeMatcher | postagCaptureNodeMatcher
    def nodeMatcher[V]: Parser[NodeMatcher[DependencyNode]] = (captureNodeMatcher | simpleNodeMatcher) ^^ { s => s.asInstanceOf[NodeMatcher[DependencyNode]] }
    
    def upEdgeMatcher = "<" ~> """[^<]+""".r <~ "<" ^^ { s => new DependencyEdgeMatcher(s, Direction.Up) }
    def downEdgeMatcher = ">" ~> """[^>]+""".r <~ ">" ^^ { s => new DependencyEdgeMatcher(s, Direction.Down) }
    def edgeMatcher[V]: Parser[EdgeMatcher[DependencyNode]] = (upEdgeMatcher | downEdgeMatcher) ^^ { s => s.asInstanceOf[EdgeMatcher[DependencyNode]] }
    
    def chain[V]: Parser[List[Matcher[DependencyNode]]] = nodeMatcher ~ edgeMatcher ~ chain ^^ { case n ~ e ~ ch => n :: e :: ch } | nodeMatcher ^^ { List(_) }
    
    def parse(s: String) = {
      parseAll(chain, s)
    }

    def apply(s: String): Pattern[DependencyNode] = {
      this.parse(s) match {
        case Success(matchers, _) => new DependencyPattern(matchers)
        case fail: Failure =>
          throw new IllegalArgumentException("improper pattern syntax. " + fail.msg + ": " + s)
      }
    }
  }

  /**
   * A more intuitive constructor that builds the pattern from a
   * bidirectional path though the tree.
   */
  def create(bipath: Bipath[DependencyNode]) = new Pattern[DependencyNode](
    bipath.path.map(dedge => new DependencyEdgeMatcher(dedge)),
    new DependencyNodeMatcher(bipath.path.head.start) with MatchLabel :: bipath.path.map(dedge => new DependencyNodeMatcher(dedge.end) with MatchLabel))

  def deserialize(string: String): Pattern[DependencyNode] = {
    Parser(string)
  }
}

/**
  * Match a `DirectedEdge[DependencyNode]`. */
class DependencyEdgeMatcher(val label: String, val dir: Direction) extends EdgeMatcher[DependencyNode] {
  def this(dedge: DirectedEdge[DependencyNode]) = this(dedge.edge.label, dedge.dir)

  override def matches(edge: DirectedEdge[DependencyNode]) =
    edge.dir == dir && edge.edge.label == label

  override def flip = new DependencyEdgeMatcher(label, dir.flip)
 
  /** symbolic representation used in serialization. */
  def symbol = dir match { 
    case Direction.Up => "<" 
    case Direction.Down => ">" 
  }

  // extend Object
  override def toString = symbol + label + symbol
  def canEqual(that: Any) = that.isInstanceOf[DependencyEdgeMatcher]
  override def equals(that: Any) = that match {
    case that: DependencyEdgeMatcher => (that canEqual this) && this.label == that.label && this.dir == that.dir
    case _ => false
  }
  override def hashCode = this.label.hashCode + 39 * this.dir.hashCode
}

abstract class AbstractDependencyNodeMatcher(val text: Option[String], val postag: Option[String]) 
extends NodeMatcher[DependencyNode] {
  override def matches(node: DependencyNode) = true
  
  // extend Object
  override def toString = text.getOrElse(postag.getOrElse(""))
  def canEqual(that: Any) = that.isInstanceOf[DependencyNodeMatcher]
  override def equals(that: Any) = that match {
    case that: DependencyNodeMatcher => (that canEqual this) && this.text == that.text && this.postag == that.postag
    case _ => false
  }
  override def hashCode = text.hashCode + 39 * postag.hashCode
}

/**
  * Match a `DependencyNode`. */
class DependencyNodeMatcher(text: Option[String], postag: Option[String]) 
extends AbstractDependencyNodeMatcher(text, postag) {
  if (!text.isDefined && !postag.isDefined)
    throw new IllegalArgumentException("either text or postag must be defined.")
  
  def this(node: DependencyNode) = this(Some(node.text), Some(node.postag))
}

/**
  * Mix-in to match the text of the `DependencyNode`. */
trait MatchLabel extends AbstractDependencyNodeMatcher {
  override def matches(node: DependencyNode) = super.matches(node) && node.text == text.getOrElse(throw new IllegalArgumentException("text must be defined"))
}

/**
  * Mix-in to match the postag of the `DependencyNode`. */
trait MatchPostag extends AbstractDependencyNodeMatcher {
  override def matches(node: DependencyNode) = 
    super.matches(node) && node.postag == postag.getOrElse(throw new IllegalArgumentException("postag must be defined"))
}

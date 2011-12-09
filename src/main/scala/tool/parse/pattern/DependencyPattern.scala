package edu.washington.cs.knowitall
package tool
package parse
package pattern

import graph._

class DependencyPattern(matchers: List[Matcher[DependencyNode]]) extends Pattern[DependencyNode](matchers) {
  def prepCount = (0 /: depEdgeMatchers) ((acc, m) => if (m.label.startsWith("prep")) 1 + acc else acc)

  def depEdgeMatchers = matchers.collect{case m: DependencyEdgeMatcher => m}
  def depNodeMatchers = matchers.collect{case m: DependencyNodeMatcher => m}
}

object DependencyPattern {
  import scala.util.parsing.combinator._

  object Parser extends RegexParsers {
    def simpleNodeMatcher = """\w+""".r ^^ { s => new DependencyNodeMatcher(Some(s), None) with MatchLabel }
    def simpleCaptureNodeMatcher = "{" ~> """\w+""".r <~ "}" ^^ { s => new CaptureNodeMatcher[DependencyNode](s) }
    def postagCaptureNodeMatcher = "{" ~> """\w+""".r ~ ":" ~ """[^}\p{Space}]+""".r <~ "}" ^^ { case s~":"~postag => new CaptureNodeMatcher[DependencyNode](s, new DependencyNodeMatcher(None, Some(postag)) with MatchPostag) { override def toString = "{" + this.alias + ":" + this.matcher.asInstanceOf[DependencyNodeMatcher].postag.get + "}"} }
    def captureNodeMatcher[V]: Parser[CaptureNodeMatcher[DependencyNode]] = simpleCaptureNodeMatcher | postagCaptureNodeMatcher
    def nodeMatcher[V]: Parser[NodeMatcher[DependencyNode]] = (captureNodeMatcher | simpleNodeMatcher) ^^ { s => s.asInstanceOf[NodeMatcher[DependencyNode]] }
    
    def upEdgeMatcher = "<" ~> """[^<]+""".r <~ "<" ^^ { s => new DependencyEdgeMatcher(s, Direction.Up) }
    def downEdgeMatcher = ">" ~> """[^>]+""".r <~ ">" ^^ { s => new DependencyEdgeMatcher(s, Direction.Down) }
    def edgeMatcher[V]: Parser[EdgeMatcher[DependencyNode]] = (upEdgeMatcher | downEdgeMatcher) ^^ { s => s.asInstanceOf[EdgeMatcher[DependencyNode]] }
    
    // def chain = nodeMatcher ~! edgeMatcher ^^ { case n ~ e => List(n, e) }
    def chain[V]: Parser[List[Matcher[DependencyNode]]] = nodeMatcher ~ edgeMatcher ~ chain ^^ { case n ~ e ~ ch => n :: e :: ch } | nodeMatcher ^^ { List(_) }
    // def expr = chain ~ nodeMatcher ^^ { case ch ~ n => ch ::: List(n)}
    
    def parse(s: String) = {
      parseAll(chain, s)
    }

    def apply(s: String): Pattern[DependencyNode] = {
      this.parse(s) match {
        case Success(matchers, _) => new Pattern[DependencyNode](matchers)
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

class DependencyEdgeMatcher(val label: String, val dir: Direction) extends EdgeMatcher[DependencyNode] {
  def this(dedge: DirectedEdge[DependencyNode]) = this(dedge.edge.label, dedge.dir)
  
  override def matches(edge: DirectedEdge[DependencyNode]) =
    edge.dir == dir && edge.edge.label == label
    
  def symbol = dir match { case Direction.Up => "<" case Direction.Down => ">" }
  override def toString = symbol + label + symbol
}

abstract class AbstractDependencyNodeMatcher(val label: Option[String], val postag: Option[String]) 
extends NodeMatcher[DependencyNode] {
  override def matches(node: DependencyNode) = true
  override def toString = label.getOrElse(postag.getOrElse(""))
}

class DependencyNodeMatcher(text: Option[String], postag: Option[String]) 
extends AbstractDependencyNodeMatcher(text, postag) {
  if (!text.isDefined && !postag.isDefined)
    throw new IllegalArgumentException("either text or postag must be defined.")
  
  def this(node: DependencyNode) = this(Some(node.text), Some(node.postag))
}

trait MatchLabel extends AbstractDependencyNodeMatcher {
  override def matches(node: DependencyNode) = super.matches(node) && node.text == label.getOrElse(throw new IllegalArgumentException("text must be defined"))
}

trait MatchPostag extends AbstractDependencyNodeMatcher {
  override def matches(node: DependencyNode) = 
    super.matches(node) && node.postag == postag.getOrElse(throw new IllegalArgumentException("postag must be defined"))
}

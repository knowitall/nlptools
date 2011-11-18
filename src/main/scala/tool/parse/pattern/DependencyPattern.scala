package edu.washington.cs.knowitall
package tool
package parse
package pattern

import graph._

object DependencyPattern {
  import scala.util.parsing.combinator._

  object Parser extends RegexParsers {
    def simpleNodeMatcher = """\w+""".r ^^ { s => new DependencyNodeMatcher(s, "") }
    def captureNodeMatcher = "{" ~> """\w+""".r <~ "}" ^^ { s => new CaptureNodeMatcher[DependencyNode](s) }
    def nodeMatcher[V]: Parser[NodeMatcher[DependencyNode]] = (captureNodeMatcher | simpleNodeMatcher) ^^ { s => s.asInstanceOf[NodeMatcher[DependencyNode]] }
    
    def upEdgeMatcher = "<" ~> """\w+""".r <~ "<" ^^ { s => new DependencyEdgeMatcher(s, Direction.Up) }
    def downEdgeMatcher = ">" ~> """\w+""".r <~ ">" ^^ { s => new DependencyEdgeMatcher(s, Direction.Down) }
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
        case e: NoSuccess =>
          System.err.println(e)
          throw new IllegalArgumentException("improper pattern syntax: " + s)
      }
    }
  }

  /**
   * A more intuitive constructor that builds the pattern from a
   * bidirectional path though the tree.
   */
  def create(bipath: Bipath[DependencyNode]) = new Pattern[DependencyNode](
    bipath.path.map(dedge => new DependencyEdgeMatcher(dedge)),
    new DependencyNodeMatcher(bipath.path.head.start) :: bipath.path.map(dedge => new DependencyNodeMatcher(dedge.end)))

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

class DependencyNodeMatcher(val label: String, val postag: String) extends NodeMatcher[DependencyNode] {
  def this(node: DependencyNode) = this(node.text, node.postag)
  
  override def matches(node: DependencyNode) = node.text == label
  override def toString = label
}

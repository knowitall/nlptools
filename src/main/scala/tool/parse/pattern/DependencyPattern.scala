package edu.washington.cs.knowitall
package tool
package parse
package pattern

import graph._
import scala.util.matching.Regex
import java.util.regex.{Pattern => JPattern}

/**
  * A pattern over a graph of dependencies. */
class DependencyPattern(matchers: List[Matcher[DependencyNode]]) 
extends Pattern[DependencyNode](matchers) {
  require(matchers != null)
  
  def this(pattern: Pattern[DependencyNode]) = this(pattern.matchers)
  
  def depEdgeMatchers: List[DependencyEdgeMatcher] = matchers.map {
    case m: DirectedEdgeMatcher[_] => m.matcher
    case m => m
  }.collect {
    case m: DependencyEdgeMatcher => m
  }
  def depNodeMatchers: List[DependencyNodeMatcher] = matchers.collect { case m: DependencyNodeMatcher => m }
}

object DependencyPattern {
  import scala.util.parsing.combinator._

  /**
    * A parser combinator for deserializing patterns over graphs of dependencies. */
  object Parser extends RegexParsers {
    def simpleNodeMatcher = """\w+""".r ^^ { s => new DependencyNodeMatcher(Some(s), None) with MatchText }
    def simpleCaptureNodeMatcher = "{" ~> """\w+""".r <~ "}" ^^ { s => new CaptureNodeMatcher[DependencyNode](s) }
    def postagCaptureNodeMatcher = "{" ~> """\w+""".r ~ ":" ~ """[^}\p{Space}]+""".r <~ "}" ^^ { case s~":"~postag => 
      new CaptureNodeMatcher[DependencyNode](s, new DependencyNodeMatcher(None, Some(postag)) with MatchPostag) {
        override def toString = "{" + this.alias + ":" + this.matcher.asInstanceOf[DependencyNodeMatcher].postag.get + "}"}
      }
    def captureNodeMatcher[V]: Parser[CaptureNodeMatcher[DependencyNode]] = simpleCaptureNodeMatcher | postagCaptureNodeMatcher
    def nodeMatcher[V]: Parser[NodeMatcher[DependencyNode]] = (captureNodeMatcher | simpleNodeMatcher) ^^ { s => s.asInstanceOf[NodeMatcher[DependencyNode]] }
    
    def regexEdgeMatcher = "regex:" ~> """[^<>{}:\p{Space}]+""".r ^^ { case regex =>
      new RegexEdgeMatcher(regex.r)
    }
    def simpleEdgeMatcher = """[^<>{}:\p{Space}]+""".r ^^ { s => new LabelEdgeMatcher(s) }
    def edgeMatcher: Parser[EdgeMatcher[DependencyNode]] = regexEdgeMatcher | simpleEdgeMatcher

    def trivialCaptureEdgeMatcher: Parser[EdgeMatcher[DependencyNode]] = "{" ~> """\w+""".r <~ "}" ^^ { case alias =>
      new CaptureEdgeMatcher(alias, new TrivialEdgeMatcher)
    }
    def complexCaptureEdgeMatcher: Parser[EdgeMatcher[DependencyNode]] = "{" ~> """\w+""".r ~ ":" ~ edgeMatcher <~ "}" ^^ { 
      case alias~":"~m =>
      new CaptureEdgeMatcher(alias, m)
    }
    def captureEdgeMatcher: Parser[EdgeMatcher[DependencyNode]] = trivialCaptureEdgeMatcher | complexCaptureEdgeMatcher

    def complexEdgeMatcher: Parser[EdgeMatcher[DependencyNode]] = edgeMatcher | captureEdgeMatcher

    def upEdgeMatcher = "<" ~> complexEdgeMatcher <~ "<" ^^ { 
      case c: CaptureEdgeMatcher[_] => new CaptureEdgeMatcher[DependencyNode](c.alias, new DirectedEdgeMatcher(Direction.Up, c.matcher))
      case m => new DirectedEdgeMatcher(Direction.Up, m)
    }
    def downEdgeMatcher = ">" ~> complexEdgeMatcher <~ ">" ^^ {  
      case c: CaptureEdgeMatcher[_] => new CaptureEdgeMatcher[DependencyNode](c.alias, new DirectedEdgeMatcher(Direction.Down, c.matcher))
      case m => new DirectedEdgeMatcher(Direction.Down, m)
    }

    def directedEdgeMatcher = upEdgeMatcher | downEdgeMatcher

    // def edgeMatcher[V]: Parser[EdgeMatcher[DependencyNode]] = (captureUpEdgeMatcherPostag | captureDownEdgeMatcherPostag | captureUpEdgeMatcher | captureDownEdgeMatcher | upEdgeMatcher | downEdgeMatcher) ^^ { s => s.asInstanceOf[EdgeMatcher[DependencyNode]] }
    
    def chain[V]: Parser[List[Matcher[DependencyNode]]] = nodeMatcher ~ directedEdgeMatcher ~ chain ^^ { case n ~ e ~ ch => n :: e :: ch } | nodeMatcher ^^ { List(_) }
    
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
    bipath.path.map(dedge => DependencyEdgeMatcher(dedge)),
    new DependencyNodeMatcher(bipath.path.head.start) with MatchText :: bipath.path.map(dedge => new DependencyNodeMatcher(dedge.end) with MatchText))

  def deserialize(string: String): Pattern[DependencyNode] = {
    try {
      Parser(string)
    }
    catch {
      case e => throw new DependencyPatternSerializationException("could not deserialize pattern: " + string, e)
    }
  }
}

class DependencyPatternSerializationException(message: String, cause: Throwable)
extends RuntimeException(message, cause)

abstract class DependencyEdgeMatcher extends EdgeMatcher[DependencyNode]
object DependencyEdgeMatcher {
  def apply(dedge: DirectedEdge[DependencyNode]) = 
    new DirectedEdgeMatcher(dedge.dir, new LabelEdgeMatcher(dedge.edge.label))
}

/**
  * Match a `DirectedEdge[DependencyNode]`. */
class LabelEdgeMatcher(val label: String) extends DependencyEdgeMatcher {
  override def matchText(edge: DirectedEdge[DependencyNode]) =
    if (label == edge.edge.label) Some(label)
    else None

  override def flip = this

  // extend Object
  override def toString = label
  def canEqual(that: Any) = that.isInstanceOf[LabelEdgeMatcher]
  override def equals(that: Any) = that match {
    case that: LabelEdgeMatcher => (that canEqual this) && this.label == that.label
    case _ => false
  }
  override def hashCode = this.label.hashCode
}

/**
  * Match a `DirectedEdge[DependencyNode]`. */
class RegexEdgeMatcher(val labelRegex: Regex) extends DependencyEdgeMatcher {
  override def matchText(edge: DirectedEdge[DependencyNode]) = edge.edge.label match {
    case labelRegex(group) => Some(group)
    case _ => None
  }

  override def flip = this

  // extend Object
  override def toString = "regex:"+labelRegex.toString
  def canEqual(that: Any) = that.isInstanceOf[RegexEdgeMatcher]
  override def equals(that: Any) = that match {
    case that: RegexEdgeMatcher => (that canEqual this) && this.labelRegex.toString == that.labelRegex.toString
    case _ => false
  }
  override def hashCode = this.labelRegex.toString.hashCode
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

  def matchText(node: DependencyNode) = if (matches(node)) Some(node.text) else None
  
  def this(node: DependencyNode) = this(Some(node.text), Some(node.postag))
}

/**
  * Mix-in to match the text of the `DependencyNode`. */
trait MatchText extends AbstractDependencyNodeMatcher {
  override def matches(node: DependencyNode) = super.matches(node) && node.text == text.getOrElse(throw new IllegalArgumentException("text must be defined"))
}

/**
  * Mix-in to match the postag of the `DependencyNode`. */
trait MatchPostag extends AbstractDependencyNodeMatcher {
  override def matches(node: DependencyNode) = 
    super.matches(node) && node.postag == postag.getOrElse(throw new IllegalArgumentException("postag must be defined"))
}

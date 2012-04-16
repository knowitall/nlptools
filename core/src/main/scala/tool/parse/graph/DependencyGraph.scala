package edu.washington.cs.knowitall
package tool
package parse
package graph

import scala.collection._
import collection.immutable.Interval
import collection.immutable.graph.Graph
import collection.immutable.graph.Graph._

/** A representation of a graph over dependencies.  
  * This richer representation may include the text of the original sentence,
  * the original nodes (before collapsing), and the original dependencies. */
class DependencyGraph (
    /** the text of the source sentence */
    val text: String,
    /** the `DependencyNode`s from the parser */
    val nodes: SortedSet[DependencyNode], 
    /** the `Dependency`s from the parser */
    val dependencies: SortedSet[Dependency],
    /** a graph representation dependencies */
    val graph: Graph[DependencyNode]
  ) {
  
  require(text != null)
  require(nodes != null)
  require(dependencies != null)
  require(graph != null)

  // check that the nodes match the nodes in the dependencies
  for (vertex <- graph.vertices) {
    nodes.find(node => vertex.indices == node.indices).map(_.text) match {
      case None => if (vertex.indices.length == 1) throw new IllegalArgumentException("no node at index: " + vertex.indices + " (" + vertex + ")")
      case Some(v) => require(v == vertex.text, "text at index " + vertex.indices + " does not match: " + vertex.text + " != " + v)
    }
  }
  
  // constructors

  def this(text: String,
      nodes: SortedSet[DependencyNode], 
      dependencies: SortedSet[Dependency]) =
    this(nodes.iterator.map(_.text).mkString(" "),
        SortedSet[DependencyNode]() ++ nodes, 
        SortedSet[Dependency]() ++ dependencies,
        new Graph[DependencyNode](dependencies.flatMap(dep => Set(dep.source, dep.dest)).toSet, dependencies))
  
  def this(text: String, 
      nodes: Iterable[DependencyNode], 
      dependencies: Iterable[Dependency]) =
    this(text, 
      SortedSet[DependencyNode]() ++ nodes, 
      SortedSet[Dependency]() ++ dependencies)

  def this(nodes: SortedSet[DependencyNode], 
      dependencies: SortedSet[Dependency]) =
    this(nodes.iterator.map(_.text).mkString(" "),
        SortedSet[DependencyNode]() ++ nodes, 
        SortedSet[Dependency]() ++ dependencies)

  def this(nodes: Iterable[DependencyNode], 
      dependencies: Iterable[Dependency]) =
    this(SortedSet[DependencyNode]() ++ nodes, 
        SortedSet[Dependency]() ++ dependencies)
  
  def canEqual(that: Any) = that.isInstanceOf[DependencyGraph]
  override def equals(that: Any) = that match {
    case that: DependencyGraph => that.canEqual(this) && 
        this.text == that.text &&
        this.nodes == that.nodes &&
        this.dependencies == that.dependencies &&
        this.graph == that.graph
    case _ => false
  }
  override def toString = serialize

  def serialize = {
    val extra = this.nodes filterNot (this.dependencies.flatMap(dep => Set(dep.source, dep.dest)).contains(_))
    
    val pickledNodes = (extra.iterator).map("("+_.serialize+")").mkString(", ")
    val pickledDeps = Dependencies.serialize(this.dependencies)

    if (pickledNodes.isEmpty) pickledDeps
    else if (pickledDeps.isEmpty) pickledNodes
    else pickledNodes + ", " + pickledDeps
  }

  def map(f: DependencyNode=>DependencyNode) = {
    val nodes = this.nodes.map(f)
    val graph = this.graph.map(f)
    val deps = this.dependencies.map(_.mapNodes(f))

    new DependencyGraph(text, nodes, deps, graph)
  }

  def collapseXNsubj =
    new DependencyGraph(this.text, this.nodes, this.dependencies,
      new Graph[DependencyNode](graph.edges.map { dep =>
        if (dep.label.equals("xsubj") || dep.label.equals("nsubj"))
          new Dependency(dep.source, dep.dest, "subj")
        else dep
      }))
  
  def collapseNNPOf = {
    def pred(edge: Edge[DependencyNode]) = (edge.source.indices distance edge.dest.indices) == 2 &&
      edge.label.equals("prep_of") && edge.source.postag == "NNP" && edge.dest.postag == "NNP"
    def merge(nodes: Traversable[DependencyNode]) = {
      if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
      val sorted = nodes.toList.sorted.view
      sorted.sliding(2).foreach(l => require((l.head.indices distance l.last.indices) == 2, "two nodes to merge don't have a distance of 2 (distance is "+(l.head.indices distance l.last.indices)+"): " + l.mkString(", ")))
      new DependencyNode(sorted.map(_.text).mkString(" of "), 
        if (nodes.forall(_.postag.equals(nodes.head.postag))) 
          nodes.head.postag
        else
          sorted.map(_.postag).mkString(" of "), Interval.span(sorted.map(_.indices)))
    }
      
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapse(pred(_))(merge))
  }
  
  /**
    * Find components that are connected by the predicate.  
    * Then, split components into subcomponents in which
    * all vertices correspond to adjacent words in the
    * source sentence. 
    */
  def adjacentComponents(pred: Edge[DependencyNode]=>Boolean): Set[Set[DependencyNode]] = {
    def splitByAdjacency(nodes: List[DependencyNode]): List[List[DependencyNode]] = {
      def rec(nodes: List[DependencyNode], result: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
        case x :: Nil => (x :: result) :: Nil
        case x :: y :: xs => if (x.indices borders y.indices) rec(y :: xs, x :: result) else (x :: result) :: rec(y :: xs, Nil)
        case Nil => Nil
      }

      rec(nodes, Nil)
    }
    
    val groups: Set[Set[DependencyNode]] = (for (dep <- graph.edges; if pred(dep)) yield {
      graph.connected(dep.source, dedge=>pred(dedge.edge))
    })(scala.collection.breakOut)
    
    
    (for {
      // for each connect nn component
      group <- groups
      // split the component by POS tag
      val nodes = group.toList.sorted
      part <- splitByAdjacency(nodes)
      if part.size > 1
    } yield(part.toSet))(scala.collection.breakOut)
  }
  
  def collapseAdjacentGroups(pred: Edge[DependencyNode]=>Boolean)
      (implicit merge: Traversable[DependencyNode]=>DependencyNode) = {
    val components = adjacentComponents(edge => pred(edge))
    val graph = this.graph.collapseGroups(components)(merge)
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph)
  }
  
  def collapseNounGroups(dividors: List[String] = List.empty) = {
    val lowerCaseDividors = dividors.map(_.toLowerCase)
    
    def pred(edge: Edge[DependencyNode]) = edge.label == "nn"
    val groups = adjacentComponents(pred)
      
    def splitByDividor(nodes: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
      case x :: xs if lowerCaseDividors.contains(x.text.toLowerCase) => List(x) :: splitByDividor(xs)
      case x :: xs => 
        val (part, rest) = nodes.span(node => !lowerCaseDividors.contains(node.text.toLowerCase))
        part :: splitByDividor(rest)
      case Nil => Nil
    }
    
    // segment ordered dependency nodes by POS tag
    def postagEqual(a: String, b: String) = a == b || a.startsWith("NNP") && b.startsWith("NNP")
    def splitByPos(nodes: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
      case x :: xs => nodes.takeWhile(node => postagEqual(node.postag, x.postag)) :: 
        splitByPos(nodes.dropWhile(node => postagEqual(node.postag, x.postag)))
      case Nil => Nil
    }
    
    val groupsToCollapse: Set[Set[DependencyNode]] = (for {
      // for each connect nn component
      group <- groups
      // split the component by POS tag
      val nodes = group.toList.sorted
      dividorSplit <- splitByDividor(nodes)
      part <- splitByPos(dividorSplit)
      if part.size > 1
    } yield(part.toSet))(scala.collection.breakOut)
    
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapseGroups(groupsToCollapse))
  }

  def directedAdjacentCollapse(labels: Set[String]): DependencyGraph = {
    def pred(edge: Edge[DependencyNode]) = labels.contains(edge.label)

    // If we get a component that is not connected, remove it from consideration.
    // It is often a mistake due to a strange parse.  It may also be an unusual edge.
    val components = adjacentComponents(pred) filter (this.graph.areConnected)
    val graph = this.graph.collapseGroups(components)(DependencyNode.directedMerge(this.graph))
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph)
  }
  
  def directedAdjacentCollapse(label: String): DependencyGraph = directedAdjacentCollapse(Set(label))

  def collapseWeakLeaves = 
    directedAdjacentCollapse(Set("neg", "det", "aux", "amod", "num", "quantmod", "advmod"))
  
  def normalize = collapseNounGroups().collapseNNPOf.simplifyPostags.collapseWeakLeaves

  def mapPostags(f: String=>String): DependencyGraph = {
    def mapNode(node: DependencyNode): DependencyNode =
      new DependencyNode(node.text, f(node.postag), node.indices)

    def mapPostags(f: String=>String) = 
      graph.map(mapNode)

    new DependencyGraph(
      this.text, 
      this.nodes.map(mapNode), 
      this.dependencies.map(_.mapNodes(mapNode)), mapPostags(f))
  }

  def simplifyPostags = {
    def simplifyPostag(postag: String) = postag match {
      // obvious winners
      case "JJS" => "JJ"
      case "NNS" => "NN"
      case "NNPS" => "NNP"
      // others can stay the same
      case x => x
    }

    mapPostags(simplifyPostag)
  }

  def simplifyVBPostags = {
    def simplifyPostag(postag: String) = postag match {
      /* VB - Verb, base form
         VBD - Verb, past tense
         VBG - Verb, gerund or present participle
         VBN - Verb, past participle
         VBP - Verb, non-3rd person singular present
         VBZ - Verb, 3rd person singular present */
      case "VB" | "VBD" | "VBG" | "VBN" | "VBP" | "VBZ" => "VERB"
      // others can stay the same
      case x => x
    }

    mapPostags(simplifyPostag)
  }
  
  def dot(title: String=this.text): String = {
    val buffer = new StringBuffer(4092)
    printDot(buffer, title)
    buffer.toString
  }
  
  def dotWithHighlights(title: String, specialNodes: Set[DependencyNode], specialEdges: Set[Edge[DependencyNode]]): String = {
    val buffer = new StringBuffer(4092)
    printDotWithHighlights(buffer, title, specialNodes, specialEdges)
    buffer.toString
  }

  def dot(title: String, 
      nodeStyle: Map[DependencyNode, String], 
      edgeStyle: Map[Edge[DependencyNode], String]): String = {
    val buffer = new StringBuffer(4092)
    printDot(buffer, title, nodeStyle, edgeStyle)
    buffer.toString
  }

  def printDot(writer: java.lang.Appendable, title: String = this.text) {
    printDot(writer, title, Map.empty, Map.empty)
  }

  def printDotWithHighlights(writer: java.lang.Appendable, title: String, specialNodes: Set[DependencyNode], specialEdges: Set[Edge[DependencyNode]]) {
    val filledNodes = specialNodes zip Stream.continually("style=filled,fillcolor=lightgray") 

    val nodeStyle = filledNodes
    val edgeStyle = (specialEdges zip Stream.continually("style=filled")) ++ 
      ((this.graph.edges -- specialEdges) zip Stream.continually("style=dotted,color=gray"))

    printDot(writer, title, nodeStyle.toMap, edgeStyle.toMap)
  }

  def printDot(writer: java.lang.Appendable, title: String, nodeStyle: Map[DependencyNode, String], edgeStyle: Map[Edge[DependencyNode], String]) {
    def quote(string: String) = "\"" + string + "\""
    def escape(string: String) = string.replaceAll("\"", "''")
    def nodeString(node: DependencyNode) = {
      val text = escape(node.text)
      val postag = escape(node.postag)
      if (graph.vertices.filter(_.text.equals(text)).size > 1) 
        text + "_" + postag + "_" + node.indices.mkString("_")
      else
        text  + "_" + postag
    }

    val indent = " " * 2;

    writer.append("digraph g {\n")

    writer.append(indent + "graph [\n")
    writer.append(indent * 2 + "fontname=\"Helvetica-Oblique\"\n")
    writer.append(indent * 2 + "fontsize=\"12\"\n")
    val cleanedTitle = title.replaceAll("\\n", "").replaceAll("\"", "'").replaceAll(";", ",")
    writer.append(indent * 2 + "label=\"" + cleanedTitle + "\"\n")
    writer.append(indent + "]\n\n")

    for (node <- this.graph.vertices.toSeq.sorted) {
      var parts: List[String] = List()
      if (nodeStyle contains node) {
        parts ::= nodeStyle(node)
      }

      val brackets = "[" + parts.mkString(",") + "]"
      writer.append(indent + quote(nodeString(node)) + " " + brackets + "\n")
    }
    writer.append("\n")
    
    for (node <- nodeStyle.keys.toSeq.sorted) {
      writer.append(indent + quote(nodeString(node)) + " [" + nodeStyle(node) + "]\n")
    }

    writer.append("\n")
    for (dep <- graph.edges.toSeq.sortBy(edge => (edge.source.indices.head, edge.dest.indices.head, edge.label))) {
      val color = dep.label match {
        case "neg" => Some("red")
        case "amod" | "advmod" => Some("lightblue")
        case "det" | "punct" => Some("lightgrey")
        case "aux" => Some("grey")
        case x if x startsWith "prep" => Some("blue")
        case _ => None
      }

      var parts = List("label=\"" + dep.label + "\"")
      if (color.isDefined) parts ::= "color=\"" + color.get + "\""
      if (edgeStyle.contains(dep)) parts ::= edgeStyle(dep)

      val brackets = "[" + parts.mkString(",") + "]"
      writer.append(indent + quote(nodeString(dep.source)) + " -> " + quote(nodeString(dep.dest)) + " " + brackets + "\n")
    }
    writer.append("}")
  }

  def printDependencies() {
    graph.outgoing.keys.foreach { key =>
      println(key + ": " + graph.outgoing(key).map(edge => edge.label + "(" + edge.dest + ")").mkString(", "))
    }
  }
}

object DependencyGraph {
  def deserialize(string: String) = {
    def rec(string: String, nodes: SortedSet[DependencyNode]): (SortedSet[DependencyNode], SortedSet[Dependency]) = {
      if (string.isEmpty) (nodes, SortedSet.empty[Dependency])
      else if (string.charAt(0) == '(') {
        val pickled = string.drop(1).takeWhile(_ != ')')
        val node = DependencyNode.deserialize(pickled)
        rec(string.drop(pickled.length + 2 /* 2 for the () */).dropWhile(_ != ',').drop(1).dropWhile(_ == ' '), nodes + node)
      }
      else (nodes, Dependencies.deserialize(string))
    }
    
    try {
      val (nodes, deps) = rec(string, SortedSet[DependencyNode]())
      val depNodes = deps.flatMap(dep => List(dep.source, dep.dest)).toSet
      new DependencyGraph((depNodes ++ nodes).iterator.map(_.text).mkString(" "), nodes ++ depNodes, deps, new Graph[DependencyNode](depNodes, deps))
    }
    catch {
      case e => throw new DependencyGraph.SerializationException(
                    "Could not deserialize graph: " + string, e)
    }
  }
  
  /** expand prep nodes that were compressed by Stanford into `DependencyNode`s. */
  private def inferCollapsedNodes(nodes: List[DependencyNode], graph: Graph[DependencyNode]): List[DependencyNode] = {
    /** at present, only infer prepositions.  Any outgoing "prep_.*" edge 
     *  means that a preposition can be inferred.  You can't restrict that
     *  the edge goes to `y` because "son of Graham Bell" has the prep_of
     *  edge between "son" and "Bell". */
    def infer(x: DependencyNode, y: DependencyNode): Option[DependencyNode] = {
      graph.outgoing(x).find { edge => 
       edge.label.startsWith("prep_")
      }.map(_.label.dropWhile(_ != '_').tail).map(text => new DependencyNode(text.replaceAll("_", " "), "IN", Interval.between(x.indices, y.indices)))
    }

    // recurse over the nodes, looking for gaps to infer
    def rec(nodes: List[DependencyNode]): List[DependencyNode] = nodes match {
      case x :: y :: tail => 
        // no gap
        if (x.indices borders y.indices) x :: rec(y :: tail) 
        // a gap of 1, let's see if we can infer
        else if (x.indices.distance(y.indices) == 2) infer(x, y) match {
          case None => x :: rec(y :: tail)
          case Some(z) => x :: z :: rec(y :: tail)
        }
        /* the gap is too large */
        else x :: rec(y :: tail)
      case tail => tail
    }

    rec(nodes)
  }

  class SerializationException(message: String, cause: Throwable) 
  extends RuntimeException(message, cause)
}

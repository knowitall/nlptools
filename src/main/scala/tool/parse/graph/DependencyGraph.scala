package edu.washington.cs.knowitall
package tool
package parse
package graph

import Graph._
import scala.collection._
import collection.immutable.Interval

class DependencyGraph(
    val text: Option[String],
    val nodes: List[DependencyNode], 
    val dependencies: List[Dependency],
    val graph: Graph[DependencyNode]
  ) {

  // check that the nodes match the nodes in the dependencies
  for (vertex <- graph.vertices) {
    nodes.find(node => vertex.indices == node.indices).map(_.text) match {
      case None => if (vertex.indices.length == 1) throw new IllegalArgumentException("no node at index: " + vertex.indices + " (" + vertex + ")")
      case Some(v) => require(v == vertex.text, "text at index " + vertex.indices + " does not match: " + vertex.text + " != " + v)
    }
  }
  
  // constructors
  
  def this(text: Option[String], nodes: List[DependencyNode], dependencies: Iterable[Dependency]) =
    this(text, nodes, dependencies.toList, new Graph[DependencyNode](DependencyNode.nodes(dependencies), dependencies))
    
  def this(text: String, nodes: List[DependencyNode], dependencies: Iterable[Dependency]) {
    this(Some(text), nodes, dependencies)
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
  
  def collapseNounGroups = {
    def pred(dedge: DirectedEdge[DependencyNode]) = dedge.edge.label.equals("nn")
    // get components connect by nn edges
    val groups: Set[Set[DependencyNode]] = (for (dep <- graph.edges; if dep.label == "nn") yield {
      graph.connected(dep.source, pred(_))
    })(scala.collection.breakOut)
    
    // segment ordered dependency nodes by POS tag
    def splitByPos(nodes: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
      case x :: xs => nodes.takeWhile(_.postag.equals(x.postag)) :: 
        splitByPos(nodes.dropWhile(_.postag.equals(x.postag)))
      case Nil => Nil
    }

    def splitByAdjacency(nodes: List[DependencyNode]): List[List[DependencyNode]] = {
      def rec(nodes: List[DependencyNode], result: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
        case x :: Nil => (x :: result) :: Nil
        case x :: y :: xs => if (x.indices borders y.indices) rec(y :: xs, x :: result) else (x :: result) :: rec(y :: xs, Nil)
        case Nil => Nil
      }

      rec(nodes, Nil)
    }
    
    val groupsToCollapse: Set[Set[DependencyNode]] = (for {
      // for each connect nn component
      group <- groups
      // split the component by POS tag
      val nodes = group.toList.sorted
      set <- splitByPos(nodes).flatMap(splitByAdjacency).map(_.toSet)
    } yield(set))(scala.collection.breakOut)
    
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapseGroups(groupsToCollapse))
  }

  def collapseDeterminers = {
    def pred(edge: Edge[DependencyNode]) = (edge.source.indices borders edge.dest.indices) && edge.label.equals("det")
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapse(pred _))
  }
  
  def normalize = collapseNounGroups.collapseNNPOf

  def simplifyGraphPostags = {
    def simplifyPostag(postag: String) = postag match {
      // obvious winners
      case "JJS" => "JJ"
      case "NNS" => "NN"
      case "NNPS" => "NNP"
      // not as clear
      
      /* VB - Verb, base form
         VBD - Verb, past tense
         VBG - Verb, gerund or present participle
         VBN - Verb, past participle
         VBP - Verb, non-3rd person singular present
         VBZ - Verb, 3rd person singular present */
      case "VB" | "VBD" | "VBG" | "VBN" | "VBP" | "VBZ" => "VB"
      // others can stay the same
      case x => x
    }
    def mapPostags(f: String=>String) = graph.map(v => new DependencyNode(v.text, f(v.postag), v.indices))
    new DependencyGraph(this.text, this.nodes, this.dependencies, mapPostags(simplifyPostag))
  }
  
  def dot(title: String): String = dot(title, Set.empty, Set.empty)
  
  def dot(title: String, filled: Set[DependencyNode], dotted: Set[Edge[DependencyNode]]): String = {
    val buffer = new StringBuffer(4092)
    printDOT(buffer, Some(title), filled, dotted)
    buffer.toString
  }

  def printDOT(writer: java.lang.Appendable, title: Option[String] = this.text) {
    printDOT(writer, title, Set.empty, Set.empty)
  }

  def printDOT(writer: java.lang.Appendable, title: Option[String], filled: Set[DependencyNode], dotted: Set[Edge[DependencyNode]]) {
    def quote(string: String) = "\"" + string + "\""
    def nodeString(node: DependencyNode) = 
      if (graph.vertices.filter(_.text.equals(node.text)).size > 1) 
        node.text + "_" + node.postag + "_" + node.indices.mkString("_")
      else
        node.text  + "_" + node.postag

    val indent = " " * 2;

    writer.append("digraph g {\n")

    writer.append(indent + "graph [\n")
    writer.append(indent * 2 + "fontname=\"Helvetica-Oblique\"\n")
    writer.append(indent * 2 + "fontsize=\"12\"\n")
    if (title.isDefined) {
      val cleanedTitle = title.get.replaceAll("\\n", "").replaceAll("\"", "'").replaceAll(";", ",")
      writer.append(indent * 2 + "label=\"" + cleanedTitle + "\"\n")
    }
    writer.append(indent + "]\n\n")

    for (node <- this.graph.vertices) {
      var parts: List[String] = List()
      if (filled contains node) {
        parts ::= "fillcolor=grey"
        parts ::= "style=filled"
      }

      if (node.postag.startsWith("NN")) {
        parts ::= "color=green"
      }
      else {
        parts ::= "color=grey"
      }

      val brackets = "[" + parts.mkString(",") + "]"
      writer.append(indent + quote(nodeString(node)) + " " + brackets + "\n")
    }
    writer.append("\n")
    
    for (node <- filled) {
      writer.append(indent + quote(nodeString(node)) + " [style=filled,fillcolor=gray]\n")
    }

    for (node <- dotted.flatMap(_.vertices)) {
      writer.append(indent + quote(nodeString(node)) + " [style=filled]\n");
    }
    
    writer.append("\n")
    for (dep <- graph.edges) {
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
      if (dotted(dep)) parts ::= "style=\"dotted\""

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

  def print() {
    def print(node: DependencyNode, indent: Int) {
      println(" " * indent + node)
      graph.outgoing(node).foreach { edge => print(edge.dest, indent + 2) }
    }

    val start = graph.vertices.find(node => graph.incoming(node).isEmpty).get
    print(start, 0)
  }
}

object DependencyGraph {
  def apply(text: Option[String], dependencies: Iterable[Dependency]): DependencyGraph = {
    val vertices = SortedSet(dependencies.flatMap(_.vertices).toSeq :_*).toList
    val graph = new Graph[DependencyNode](vertices, dependencies)
    val nodes = inferCollapsedNodes(vertices, graph)
    new DependencyGraph(text, nodes, dependencies.toList, graph)
  }

  def apply(text: String, dependencies: Iterable[Dependency]): DependencyGraph =
    apply(Some(text), dependencies)

  def apply(dependencies: Iterable[Dependency]): DependencyGraph =
    apply(None, dependencies)

  private def inferCollapsedNodes(nodes: List[DependencyNode], graph: Graph[DependencyNode]): List[DependencyNode] = {
    /** at present, only infer prepositions.  Any outgoing "prep_.*" edge 
     *  means that a preposition can be inferred.  You can't restrict that
     *  the edge goes to `y` because "son of Graham Bell" has the prep_of
     *  edge between "son" and "Bell". */
    def infer(x: DependencyNode, y: DependencyNode): Option[DependencyNode] = {
      graph.outgoing(x).find { edge => 
       edge.label.startsWith("prep_")
      }.map(_.label.dropWhile(_ != '_').tail).map(text => new DependencyNode(text, "IN", Interval.between(x.indices, y.indices)))
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
}

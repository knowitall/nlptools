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
  
  // constructors
  
  def this(text: Option[String], nodes: List[DependencyNode], dependencies: Iterable[Dependency]) =
    this(text, nodes, dependencies.toList, new Graph[DependencyNode](DependencyNode.nodes(dependencies), dependencies))
    
  def this(text: String, nodes: List[DependencyNode], dependencies: Iterable[Dependency]) {
    this(Some(text), nodes, dependencies)
  }

  def this(text: Option[String], dependencies: Iterable[Dependency]) =
    this(text, dependencies.flatMap(_.vertices).toSet.toList.sorted, dependencies)

  def this(text: String, dependencies: Iterable[Dependency]) =
    this(Some(text), dependencies.flatMap(_.vertices).toSet.toList.sorted, dependencies)

  def this(dependencies: Iterable[Dependency]) =
    this(None, dependencies)

  def collapseXNsubj =
    new DependencyGraph(this.text, this.nodes, this.dependencies,
      new Graph[DependencyNode](graph.edges.map { dep =>
        if (dep.label.equals("xsubj") || dep.label.equals("nsubj"))
          new Dependency(dep.source, dep.dest, "subj")
        else dep
      }))
  
  def collapseNNPOf() = {
    def pred(edge: Edge[DependencyNode]) =
      edge.label.equals("prep_of") && edge.source.postag == "NNP" && edge.dest.postag == "NNP"
    def merge(nodes: Traversable[DependencyNode]) = {
      if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
      val sorted = nodes.toList.sorted.view
      new DependencyNode(sorted.map(_.text).mkString(" of "), 
        if (nodes.forall(_.postag.equals(nodes.head.postag))) 
          nodes.head.postag
        else
          sorted.map(_.postag).mkString(" of "), Interval.union(sorted.map(_.indices)))
    }
      
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapse(pred(_))(merge))
  }
  
  def collapseNounGroups = {
    def pred(dedge: DirectedEdge[DependencyNode]) = dedge.edge.label.equals("nn")
    var groups: Set[Set[DependencyNode]] = Set()
    for (dep <- graph.edges) {
      if (dep.label.equals("nn")) {
        groups += graph.connected(dep.source, pred(_))
      }
    }
    
    def splitByPos(nodes: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
        case x :: xs => nodes.takeWhile(_.postag.equals(x.postag)) :: 
          splitByPos(nodes.dropWhile(_.postag.equals(x.postag)))
        case Nil => Nil
    }
    
    var map: Map[DependencyNode, Set[DependencyNode]] = Map()
    for (group <- groups) {
      val nodes = group.toList.sorted
      val sets = splitByPos(nodes).map(new mutable.HashSet[DependencyNode]() ++ _)
      for (set <- sets) {
        for (node <- set) {
          map += node -> set
        }
      }
    }
    
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapseGroups(map.values))
  }
  
  def collapseNN = {
    def pred(edge: Edge[DependencyNode]) = { println(edge.source.indices + " & " + edge.dest.indices + ":" + edge.source.indices.borders(edge.dest.indices))
      edge.label.equals("nn") && 
      edge.source.indices.borders(edge.dest.indices) &&
      (edge.source.isProperNoun && edge.dest.isProperNoun || edge.source.isCommonNoun && edge.dest.isCommonNoun)
    }

    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapse(pred(_)))
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

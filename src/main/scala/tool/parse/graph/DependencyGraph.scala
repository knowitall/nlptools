package edu.washington.cs.knowitall
package tool
package parse
package graph

import scala.collection._

class DependencyGraph(
    val sentence: Option[String], 
    val graph: Graph[DependencyNode]
  ) {
  
  // constructors
  
  def this(sentence: Option[String], dependencies: Iterable[Dependency]) =
    this(sentence, new Graph[DependencyNode](DependencyNode.nodes(dependencies), dependencies))

  def this(sentence: String, dependencies: Iterable[Dependency]) =
    this(Some(sentence), new Graph[DependencyNode](dependencies))

  def this(dependencies: Iterable[Dependency]) =
    this(None, new Graph[DependencyNode](dependencies))
    
  def collapseXNsubj =
    new DependencyGraph(this.sentence,
      new Graph[DependencyNode](graph.edges.map { dep =>
        if (dep.label.equals("xsubj") || dep.label.equals("nsubj"))
          new Dependency(dep.source, dep.dest, "subj")
        else dep
      }))
  
  def collapseNNPOf() = {
    def pred(edge: Edge[DependencyNode]) =
      edge.label.equals("prep_of") && edge.source.pos == "NNP" && edge.dest.pos == "NNP"
    def merge(nodes: List[DependencyNode]) = {
      if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
      val sorted = nodes.sortBy(_.index).view
      new DependencyNode(sorted.map(_.text).mkString(" of "), 
        if (nodes.forall(_.pos.equals(nodes.head.pos))) 
          nodes.head.pos
        else
          sorted.map(_.pos).mkString(" of "), sorted.head.index)
    }
      
    new DependencyGraph(sentence, graph.collapse(pred, merge))
  }
  
  def collapseNounGroups = {
    def pred(edge: Edge[DependencyNode]) = edge.label.equals("nn")
    var groups: Set[Set[DependencyNode]] = Set()
    for (dep <- graph.edges) {
      if (dep.label.equals("nn")) {
        groups += graph.connected(dep.source, pred)
      }
    }
    
    def splitByPos(nodes: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
        case x :: xs => nodes.takeWhile(_.pos.equals(x.pos)) :: 
          splitByPos(nodes.dropWhile(_.pos.equals(x.pos)))
        case Nil => Nil
    }
    
    var map: Map[DependencyNode, Set[DependencyNode]] = Map()
    for (group <- groups) {
      val nodes = group.toList.sortBy(_.index)
      val sets = splitByPos(nodes).map(new mutable.HashSet[DependencyNode]() ++ _)
      for (set <- sets) {
        for (node <- set) {
          map += node -> set
        }
      }
    }
    
    new DependencyGraph(sentence, graph.collapseVertices(map, DependencyNode.merge))
  }
  
  def collapseNN =
    new DependencyGraph(sentence, graph.collapse(_.label.equals("nn"), DependencyNode.merge))
  
  def dot(title: String): String = dot(title, Set.empty, Set.empty)
  
  def dot(title: String, filled: Set[DependencyNode], dotted: Set[Edge[DependencyNode]]): String = {
    val buffer = new StringBuffer(4092)
    printDOT(buffer, title, filled, dotted)
    buffer.toString
  }

  def printDOT(writer: java.lang.Appendable, title: String = this.sentence.get) {
    printDOT(writer, title, Set.empty, Set.empty)
  }

  def printDOT(writer: java.lang.Appendable, title: String, filled: Set[DependencyNode], dotted: Set[Edge[DependencyNode]]) {
    def quote(string: String) = "\"" + string + "\""
    def nodeString(node: DependencyNode) = 
      if (graph.nodes.filter(_.text.equals(node.text)).size > 1) 
        node.text + "_" + node.pos + "_" + node.index
      else
        node.text  + "_" + node.pos

    val indent = " " * 2;

    writer.append("digraph g {\n")

    if (this.sentence.isDefined) {
      val cleanedTitle = title.replaceAll("\\n", "").replaceAll("\"", "'").replaceAll(";", ",")
      writer.append(indent + "graph [\n")
      writer.append(indent * 2 + "fontname=\"Helvetica-Oblique\"\n")
      writer.append(indent * 2 + "fontsize=\"12\"\n")
      writer.append(indent * 2 + "label=\"" + cleanedTitle + "\"\n")
      writer.append(indent + "]\n\n")
    }

    writer.append(indent + "node [\n")
    writer.append(indent * 2 + "color=gray\n")
    writer.append(indent * 2 + "fillcolor=lightgray\n")
    writer.append(indent + "]\n\n")
    
    for (node <- filled) {
      writer.append(indent + quote(nodeString(node)) + " [style=filled,fillcolor=gray]\n")
    }

    for (node <- dotted.flatMap(_.vertices)) {
      writer.append(indent + quote(nodeString(node)) + " [style=filled]\n");
    }
    
    writer.append("\n")
    for (dep <- graph.edges) {
      val brackets = "[label=\"" + dep.label + "\"" + { if (dotted(dep)) ",style=\"dotted\"" else "" } + "]"
      writer.append(indent + quote(nodeString(dep.source)) + " -> " + quote(nodeString(dep.dest)) + " " + brackets + "\n")
    }
    writer.append("}")
  }

  def printDependencies() {
    graph._outgoing.keys.foreach { key =>
      println(key + ": " + graph.outgoing(key).map(edge => edge.label + "(" + edge.dest + ")").mkString(", "))
    }
  }

  def print() {
    def print(node: DependencyNode, indent: Int) {
      println(" " * indent + node)
      graph.outgoing(node).foreach { edge => print(edge.dest, indent + 2) }
    }

    val start = graph.nodes.find(node => graph.incoming(node).isEmpty).get
    print(start, 0)
  }
}

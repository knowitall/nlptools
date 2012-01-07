package edu.washington.cs.knowitall
package tool
package parse
package graph

import Graph._
import scala.collection._
import collection.immutable.Interval

/** A representation of a graph over dependencies.  
  * This richer representation may include the text of the original sentence,
  * the original nodes (before collapsing), and the original dependencies. */
class DependencyGraph(
    /** the text of the source sentence */
    val text: Option[String],
    /** the `DependencyNode`s from the parser */
    val nodes: List[DependencyNode], 
    /** the `Dependency`s from the parser */
    val dependencies: List[Dependency],
    /** a graph representation dependencies */
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
    this(text, nodes, dependencies.toList, new Graph[DependencyNode](dependencies.flatMap(dep => Set(dep.source, dep.dest)).toSet, dependencies))
    
  def this(text: String, nodes: List[DependencyNode], dependencies: Iterable[Dependency]) {
    this(Some(text), nodes, dependencies)
  }
  
  def serialize = {
    val extra = this.nodes filterNot (this.dependencies.flatMap(dep => Set(dep.source, dep.dest)).contains(_))
    val deps = Dependencies.serialize(this.dependencies)
    
    Iterable(extra.map("("+_+")").mkString(", "), deps).mkString(", ")
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
  
  def collapseNounGroups(dividors: List[String] = List.empty) = {
    val lowerCaseDividors = dividors.map(_.toLowerCase)
    
    def pred(dedge: DirectedEdge[DependencyNode]) = dedge.edge.label.equals("nn")
    // get components connect by nn edges
    
    val groups: Set[Set[DependencyNode]] = (for (dep <- graph.edges; if dep.label == "nn") yield {
      graph.connected(dep.source, pred(_))
    })(scala.collection.breakOut)
    
    def splitByDividor(nodes: List[DependencyNode]): List[List[DependencyNode]] = nodes match {
      case x :: xs if lowerCaseDividors.contains(x.text.toLowerCase) => List(x) :: splitByDividor(xs)
      case x :: xs => 
        val (part, rest) = nodes.span(node => !lowerCaseDividors.contains(node.text.toLowerCase))
        part :: splitByDividor(rest)
      case Nil => Nil
    }
    
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
      dividorSplit <- splitByDividor(nodes)
      posSplit <- splitByPos(dividorSplit)
      part <- splitByAdjacency(posSplit)
    } yield(part.toSet))(scala.collection.breakOut)
    
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapseGroups(groupsToCollapse))
  }

  def collapseDeterminers = {
    def pred(edge: Edge[DependencyNode]) = (edge.source.indices borders edge.dest.indices) && edge.label.equals("det")
    new DependencyGraph(this.text, this.nodes, this.dependencies, graph.collapse(pred _))
  }
  
  def normalize = collapseNounGroups().collapseNNPOf.simplifyPostags

  def mapPostags(f: String=>String): DependencyGraph = {
    def mapPostags(f: String=>String) = 
      graph.map(v => new DependencyNode(v.text, f(v.postag), v.indices))

    new DependencyGraph(
      this.text, 
      this.nodes, 
      this.dependencies, mapPostags(f))
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
}

object DependencyGraph {
  def apply(text: Option[String], dependencies: Iterable[Dependency]): DependencyGraph = {
    val vertices = SortedSet(dependencies.flatMap(_.vertices).toSeq :_*).toList
    text match {
      // use the text to fill in the missing nodes
      case Some(text) =>
        val dependencyNodes = dependencies.flatMap(dep => List(dep.source, dep.dest))
        val nodes: List[DependencyNode] = text.split("\\s+").zipWithIndex.map { case (s, i) =>
          dependencyNodes.find(dep => dep.indices.start == i) match {
            case Some(node) => node
            case None => new DependencyNode(s, null, Interval.singleton(i))
          }
        }(scala.collection.breakOut)
        
        val graph = new Graph[DependencyNode](vertices, dependencies)
        new DependencyGraph(Some(text), nodes, dependencies.toList, graph)
      // infer the missing nodes
      case None =>
        val graph = new Graph[DependencyNode](vertices, dependencies)
        val nodes = inferCollapsedNodes(vertices, graph)
        new DependencyGraph(text, nodes, dependencies.toList, graph)
    }
  }

  def apply(text: String, dependencies: Iterable[Dependency]): DependencyGraph =
    apply(Some(text), dependencies)

  def apply(dependencies: Iterable[Dependency]): DependencyGraph =
    apply(None, dependencies)

  def deserialize(string: String) = {
    def rec(string: String, nodes: List[DependencyNode]): (List[DependencyNode], List[Dependency]) = {
      if (string.charAt(0) == '(') {
        val pickled = string.drop(1).takeWhile(_ != ')')
        val node = DependencyNode.deserialize(pickled)
        rec(string.dropWhile(_ != ',').drop(1).dropWhile(_ == ' '), node :: nodes)
      }
      else (nodes.reverse, Dependencies.deserialize(string))
    }
    
    val (nodes, deps) = rec(string, List())
    new DependencyGraph(None, nodes, deps, new Graph[DependencyNode](nodes, deps))
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
}

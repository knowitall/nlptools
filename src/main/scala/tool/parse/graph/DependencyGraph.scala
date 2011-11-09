package edu.washington.cs.knowitall
package tool
package parse
package graph

import scala.collection._

sealed trait Direction {
  def name: String
}
object Direction {
  case object Up extends Direction {
    override def name = "Up"
  }
  case object Down extends Direction {
    override def name = "Down"
  }
}

sealed abstract class DirectedEdge(val edge: Dependency) {
  def start: DependencyNode
  def end: DependencyNode
  def dir: Direction
  def switchStart(newStart: DependencyNode): DirectedEdge
  def switchEnd(newEnd: DependencyNode): DirectedEdge
  def flip: DirectedEdge
  
  override def toString() = edge.toString
}

class UpEdge(edge: Dependency) extends DirectedEdge(edge) {
  def start = edge.dest
  def end = edge.source
  def dir = Direction.Up
  def switchStart(newStart: DependencyNode) =
    new UpEdge(new Dependency(edge.source, newStart, edge.label))
  def switchEnd(newEnd: DependencyNode) =
    new UpEdge(new Dependency(newEnd, edge.dest, edge.label))
  def flip = new DownEdge(edge)
  
  override def toString() = "Up(" + super.toString + ")"
}

class DownEdge(edge: Dependency) extends DirectedEdge(edge) {
  def start = edge.source
  def end = edge.dest
  def dir = Direction.Down
  def switchStart(newStart: DependencyNode) =
    new DownEdge(new Dependency(newStart, edge.dest, edge.label))
  def switchEnd(newEnd: DependencyNode) =
    new DownEdge(new Dependency(edge.source, newEnd, edge.label))
  def flip = new UpEdge(edge)
  
  override def toString() = "Down(" + super.toString + ")"
}

class Bipath(val path: List[DirectedEdge]) {
  def edges = path.foldRight[Set[Dependency]](Set()) { case (item, set) => set + item.edge }
  def nodes = path.head.start :: path.map(_.end)
  def start = path.head.start
  def collapse(pred: Dependency=>Boolean) = {
    if (path.forall(dep => pred(dep.edge))) {
      this
    } else {
      val array = path.toArray
      for (i <- array.indices) {
        val current = array(i)
        if (pred(current.edge)) {
          val merge = DependencyNode.merge(List(current.start, current.end))
          if (current.isInstanceOf[UpEdge]) {
            if (array.indices contains (i + 1)) {
              array(i + 1) = array(i + 1).switchStart(merge)
            }

            if (array.indices contains (i - 1)) {
              array(i - 1) = array(i - 1).switchEnd(merge)
            }
          } else if (current.isInstanceOf[DownEdge]) {
            if (array.indices contains (i + 1)) {
              array(i + 1).switchStart(merge)
            }

            if (array.indices contains (i - 1)) {
              array(i - 1) = array(i - 1).switchEnd(merge)
            }
          }
        }
      }

      new Bipath(array.filter(dep => !pred(dep.edge)).toList)
    }
  }
  def collapseNN = {
    collapse(_.label.equals("nn"))
  }
  def collapseHeuristic = {
    collapse(
      edge => 
        edge.label == "nn" && edge.source.pos.equals(edge.dest.pos) ||
        edge.label == "prep_of" && edge.source.pos.equals("NNP") && edge.dest.pos.equals("NNP"))
  }
  override def toString = "[" + path.mkString(", ") + "]";
}

class DependencyGraph(val sentence: Option[String], 
  val vertices: Iterable[DependencyNode], 
  val dependencies: Iterable[Dependency]) {

  val _outgoing = new mutable.HashMap[DependencyNode, mutable.Set[Dependency]]() 
    with mutable.MultiMap[DependencyNode, Dependency]
  val _incoming = new mutable.HashMap[DependencyNode, mutable.Set[Dependency]]() 
    with mutable.MultiMap[DependencyNode, Dependency]
  val nodes = dependencies.map(dep => dep.source).toSet union dependencies.map(dep => dep.dest).toSet
  dependencies.foreach { dep => _outgoing.addBinding(dep.source, dep) }
  dependencies.foreach { dep => _incoming.addBinding(dep.dest, dep) }

  def this(sentence: Option[String], dependencies: Iterable[Dependency]) =
    this(sentence, DependencyNode.nodes(dependencies), dependencies)

  def this(sentence: String, dependencies: Iterable[Dependency]) =
    this(Some(sentence), dependencies)

  def this(dependencies: Iterable[Dependency]) =
    this(None, dependencies)
  
  def expand(deps: Set[DependencyNode], pred: Dependency=>Boolean) = {
    deps.flatMap { node => neighbors(node, pred) + node }
  }
  
  def connected(dep: DependencyNode, pred: Dependency=>Boolean) = {
    var nodes = Set(dep)
    var last: Set[DependencyNode] = Set.empty
    while (nodes.size > last.size) {
      last = nodes
      nodes = expand(nodes, pred)
    }
    
    nodes
  }

  def collapseXNsubj =
    new DependencyGraph(this.sentence,
      dependencies.map { dep =>
        if (dep.label.equals("xsubj") || dep.label.equals("nsubj"))
          new Dependency(dep.source, dep.dest, "subj")
        else dep
      })
  
  def collapseNNPOf() = {
    def pred(dep: Dependency) =
      dep.label.equals("prep_of") && dep.source.pos == "NNP" && dep.dest.pos == "NNP"
    def merge(nodes: List[DependencyNode]) = {
      if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
      val sorted = nodes.sortBy(_.index).view
      new DependencyNode(sorted.map(_.text).mkString(" of "), 
        if (nodes.forall(_.pos.equals(nodes.head.pos))) 
          nodes.head.pos
        else
          sorted.map(_.pos).mkString(" of "), sorted.head.index)
    }
      
    collapse(pred, merge)
  }
  
  def collapseNounGroups = {
    def pred(dep: Dependency) = dep.label.equals("nn")
    var groups: Set[Set[DependencyNode]] = Set()
    for (dep <- this.dependencies) {
      if (dep.label.equals("nn")) {
        groups += connected(dep.source, pred)
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
    
    collapseNodes(map)
  }
  
  def collapseNN =
    collapse(_.label.equals("nn"))

  def collapse(collapsable: Dependency => Boolean): DependencyGraph = {
    collapse(collapsable, DependencyNode.merge)
  }
  
  def collapse(collapsable: Dependency => Boolean, merge: List[DependencyNode] => DependencyNode): DependencyGraph = {
    // find nn edges
    val (nndeps, otherdeps) = dependencies.partition(collapsable)

    // collapse edges by building a map from collapsed nodes
    // to collections of joined nodes
    var map: Map[DependencyNode, mutable.Set[DependencyNode]] = Map()
    for (dep <- nndeps) {
      // dest is already collapsed
      if (map.contains(dep.dest)) {
        map(dep.dest) += dep.source
        map += dep.source -> map(dep.dest)
      } // source is already collapsed
      else if (map.contains(dep.source)) {
        map(dep.source) += dep.dest
        map += dep.dest -> map(dep.source)
      } // neither is collapsed
      else {
        val set = new mutable.HashSet[DependencyNode]()
        set += dep.source
        set += dep.dest
        map += dep.dest -> set
        map += dep.source -> set
      }
    }
    
    collapseNodes(map, merge)
  }
  
  def collapseNodes(groups: Map[DependencyNode, Set[DependencyNode]]): DependencyGraph = 
    collapseNodes(groups, DependencyNode.merge)
  
  def collapseNodes(groups: Map[DependencyNode, Set[DependencyNode]], merge: List[DependencyNode] => DependencyNode) = {
    // convert collapsed nodes to a single DependencyNode
    val transformed = groups.values.flatMap { deps =>
      val sorted = deps.toList.sortBy(_.index)
      deps.map { dep => (dep, merge(sorted)) }
    }.toMap
    
    // map other dependencies to the new DependencyNodes
    val newdeps = dependencies.flatMap { dep =>
      val tsource = transformed.get(dep.source)
      val tdest = transformed.get(dep.dest)
      
      val source = tsource.getOrElse(dep.source)
      val dest = tdest.getOrElse(dep.dest)
      
      if (source == dest) List()
      else List(new Dependency(source, dest, dep.label))
    }

    new DependencyGraph(this.sentence, newdeps)
  }

  def outgoing(node: DependencyNode): mutable.Set[Dependency] =
    _outgoing.getOrElse(node, mutable.HashSet.empty)

  def incoming(node: DependencyNode): mutable.Set[Dependency] =
    _incoming.getOrElse(node, mutable.HashSet.empty)

  def edges(node: DependencyNode): immutable.Set[Dependency] = (outgoing(node).toSet) union (incoming(node).toSet)

  def dedges(node: DependencyNode): immutable.Set[DirectedEdge] = 
    outgoing(node).map(new DownEdge(_): DirectedEdge).union(
      incoming(node).map(new UpEdge(_): DirectedEdge)).toSet
    
  def neighbors(node: DependencyNode, pred: Dependency=>Boolean): Set[DependencyNode] =
    outgoing(node).filter(pred).map(edge => edge.dest) union incoming(node).filter(pred).map(edge => edge.source)

  def neighbors(node: DependencyNode): Set[DependencyNode] = neighbors(node, Any=>true)

  def inferiors(node: DependencyNode): List[DependencyNode] =
    node :: outgoing(node).map(edge => inferiors(edge.dest)).toList.flatten

  private def toBipath(nodes: List[DependencyNode]) = {
    def toEdgeBipath(nodes: List[DependencyNode]): List[List[DirectedEdge]] = nodes match {
      case a :: b :: xs =>
        val out = outgoing(a)
        val in = incoming(a)
        val outedge = out.find(edge => edge.dest.equals(b)).map(edge => new DownEdge(edge))
        val inedge = in.find(edge => edge.source.equals(b)).map(edge => new UpEdge(edge))
        val edge = outedge.getOrElse(inedge.getOrElse(throw new IllegalArgumentException))
        (outedge ++ inedge).flatMap(edge => toEdgeBipath(b :: xs).map(path => edge :: path)).toList
      case _ => List(List())
    }
    toEdgeBipath(nodes).map(new Bipath(_))
  }

  def edgeBipaths(start: DependencyNode, end: DependencyNode): List[Bipath] = {
    val nodePaths = bipaths(start, end)
    nodePaths.flatMap(np => toBipath(np))
  }

  def edgeBipaths(nodes: Set[DependencyNode]): Set[Bipath] = {
    val nodePaths = bipaths(nodes)
    nodePaths.flatMap(np => toBipath(np))
  }

  /**
   * Find a path from node (start) to node (end).
   */
  def bipaths(start: DependencyNode, end: DependencyNode): List[List[DependencyNode]] = {
    def bipaths(start: DependencyNode, path: List[DependencyNode]): List[List[DependencyNode]] = {
      if (start.equals(end)) List(path)
      else neighbors(start).filter(nb => !path.contains(nb)).toList.flatMap(nb => bipaths(nb, nb :: path))
    }

    bipaths(start, List(start)).map(_.reverse)
  }

  /**
   * Find a path that contains all nodes in (nodes).
   */
  def bipaths(nodes: Set[DependencyNode]): Set[List[DependencyNode]] = {
    def bipaths(start: DependencyNode, path: List[DependencyNode]): List[List[DependencyNode]] = {
      if (nodes.forall(path.contains(_))) List(path)
      else neighbors(start).filter(nb => !path.contains(nb)).toList.flatMap(nb => bipaths(nb, nb :: path))
    }

    nodes.flatMap(start => bipaths(start, List(start)).map(_.reverse))
  }

  def contents(node: DependencyNode): List[String] = inferiors(node).sortBy(node => node.index).map(node => node.text)
  
  def dot(title: String): String = dot(title, Set.empty, Set.empty)
  
  def dot(title: String, filled: Set[DependencyNode], dotted: Set[Dependency]): String = {
    val buffer = new StringBuffer(4092)
    printDOT(buffer, title, filled, dotted)
    buffer.toString
  }

  def printDOT(writer: java.lang.Appendable, title: String = this.sentence.get) {
    printDOT(writer, title, Set.empty, Set.empty)
  }

  def printDOT(writer: java.lang.Appendable, title: String, filled: Set[DependencyNode], dotted: Set[Dependency]) {
    def quote(string: String) = "\"" + string + "\""
    def nodeString(node: DependencyNode) = 
      if (this.nodes.filter(_.text.equals(node.text)).size > 1) 
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

    for (node <- dotted.flatMap(_.nodes)) {
      writer.append(indent + quote(nodeString(node)) + " [style=filled]\n");
    }
    
    writer.append("\n")
    for (dep <- dependencies) {
      val brackets = "[label=\"" + dep.label + "\"" + { if (dotted(dep)) ",style=\"dotted\"" else "" } + "]"
      writer.append(indent + quote(nodeString(dep.source)) + " -> " + quote(nodeString(dep.dest)) + " " + brackets + "\n")
    }
    writer.append("}")
  }

  def printDependencies() {
    _outgoing.keys.foreach { key =>
      println(key + ": " + outgoing(key).map(edge => edge.label + "(" + edge.dest + ")").mkString(", "))
    }
  }

  def print() {
    def print(node: DependencyNode, indent: Int) {
      println(" " * indent + node)
      outgoing(node).foreach { edge => print(edge.dest, indent + 2) }
    }

    val start = nodes.find(node => incoming(node).isEmpty).get
    print(start, 0)
  }
}

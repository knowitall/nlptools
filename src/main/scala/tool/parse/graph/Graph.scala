package edu.washington.cs.knowitall
package tool
package parse
package graph

import scala.collection._
import Graph._

class Graph[T] (
    val vertices: Iterable[T], 
    val edges: Iterable[Edge[T]]
  ) {

  type G = Graph[T]
  type E = Edge[T]

  // fields

  val _outgoing = new mutable.HashMap[T, mutable.Set[E]]() 
    with mutable.MultiMap[T, E]
  val _incoming = new mutable.HashMap[T, mutable.Set[E]]() 
    with mutable.MultiMap[T, E]
  val nodes = edges.map(dep => dep.source).toSet union edges.map(dep => dep.dest).toSet
  edges.foreach { dep => _outgoing.addBinding(dep.source, dep) }
  edges.foreach { dep => _incoming.addBinding(dep.dest, dep) }

  // constructors
  
  def this(edges: Iterable[Edge[T]]) =
    this(Graph.vertices(edges), edges)
  
  /* Expand a set of `Vertex`s to all neighbors along immediate edges
   * that satisfy the supplied predicate. 
   * 
   * @param  deps  the seed `Vertex`s
   * @param  pred  the predicate edges must match to be expanded upon
   * @return  the  the set of `Vertex`s in the expansion
   */
  def expand(vertices: Set[T], pred: DirectedEdge[T]=>Boolean) = {
    vertices ++ vertices.flatMap { node => neighbors(node, pred) }
  }
  
  /* Iteratively expand the neighbors of a `Vertex` to all
   * neighbors along an edge that satisfy the supplied predicate.
   * 
   * @param  dep  the seed `Vertex`
   * @param  pred  the predicate edges must match to be expanded upon
   * @return  the set of `Vertex`s in the expansion
   */
  def connected(v: T, pred: DirectedEdge[T]=>Boolean): Set[T] = {
    def rec(vertices: Set[T], last: Set[T]): Set[T] = {
      val neighbors: Set[T] = last.flatMap(this.neighbors(_, pred))
      if (neighbors.isEmpty) vertices
      else rec(vertices ++ neighbors, neighbors -- vertices)
    }

    rec(Set(v), Set(v))
  }

  def collapse(collapsable: E => Boolean)(implicit merge: Traversable[T] => T): G = {
    // find nn edges
    val (nndeps, otherdeps) = edges.partition(collapsable)

    // collapse edges by building a map from collapsed nodes
    // to collections of joined nodes
    var map: Map[T, mutable.Set[T]] = Map()
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
        val set = new mutable.HashSet[T]()
        set += dep.source
        set += dep.dest
        map += dep.dest -> set
        map += dep.source -> set
      }
    }
    
    collapseGroups(map.values)(merge)
  }

  def collapse(set: Set[T])(implicit merge: Traversable[T] => T) = collapseGroups(Iterable(set))
  
  def collapseGroups(groups: Iterable[Set[T]])(implicit merge: Traversable[T] => T) = {
    // convert collapsed nodes to a single Vertex
    val transformed = groups.flatMap { vertices =>
      vertices.map { dep => (dep, merge(vertices)) }
    }.toMap
    
    // map other edges to the new vertices
    val newdeps = edges.flatMap { dep =>
      val tsource = transformed.get(dep.source)
      val tdest = transformed.get(dep.dest)
      
      val source = tsource.getOrElse(dep.source)
      val dest = tdest.getOrElse(dep.dest)
      
      if (source == dest) List()
      else List(new E(source, dest, dep.label))
    }

    new Graph(newdeps)
  }

  def outgoing(node: T): immutable.Set[E] =
    _outgoing.getOrElse(node, mutable.HashSet.empty).toSet

  def incoming(node: T): immutable.Set[E] =
    _incoming.getOrElse(node, mutable.HashSet.empty).toSet

  def edges(node: T): immutable.Set[E] = (outgoing(node).toSet) union (incoming(node).toSet)

  def dedges(node: T): immutable.Set[DirectedEdge[T]] = 
    outgoing(node).map(new DownEdge(_): DirectedEdge[T]).union(
      incoming(node).map(new UpEdge(_): DirectedEdge[T])).toSet
    
  def neighbors(v: T, pred: DirectedEdge[T]=>Boolean): Set[T] =
    dedges(v).withFilter(pred).map { _ match { 
      case out: DownEdge[_] =>  out.end
      case in: UpEdge[_] =>  in.end
    }}

  def neighbors(v: T): Set[T] = predecessors(v) union successors(v)

  def predecessors(v: T) = incoming(v).map(edge => edge.source)

  def successors(v: T) = outgoing(v).map(edge => edge.dest)

  /* Iteratively expand a vertex to all nodes beneath it. 
   * 
   * @param  vertex  the seed vertex
   * @return  the set of vertices beneath `vertex`
   */
  def inferiors(v: T, cond: E=>Boolean = (x=>true)): Set[T] = {
    def conditional(dedge: DirectedEdge[T]) = dedge match {
      case down: DownEdge[_] => cond(down.edge)
      case _: UpEdge[_] => false
    }
    connected(v, conditional)
  }

  /* Iteratively expand a vertex to all nodes above it. 
   * 
   * @param  vertex  the seed vertex 
   * @return  the set of vertices beneath `vertex`
   */
  def superiors(v: T, cond: E=>Boolean = (x=>true)): Set[T] = {
    def conditional(dedge: DirectedEdge[T]) = dedge match {
      case up: UpEdge[_] => cond(up.edge)
      case _: DownEdge[_] => false
    }
    connected(v, conditional)
  }

  /* number of out-edges bordering v */
  def outdegree(v: T) = outgoing(v).size
  /* number of in-edges bordering v */
  def indegree(v: T) = incoming(v).size
  /* number of edges bordering v */
  def degree(v: T) = indegree(v) + outdegree(v)

  private def toBipath(nodes: List[T]) = {
    def toEdgeBipath(nodes: List[T]): List[List[DirectedEdge[T]]] = nodes match {
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

  def edgeBipaths(start: T, end: T): List[Bipath[T]] = {
    val nodePaths = bipaths(start, end)
    nodePaths.flatMap(np => toBipath(np))
  }

  def edgeBipaths(nodes: Set[T]): Set[Bipath[T]] = {
    val nodePaths = bipaths(nodes)
    nodePaths.flatMap(np => toBipath(np))
  }

  /**
   * Find a path from node (start) to node (end).
   */
  def bipaths(start: T, end: T): List[List[T]] = {
    def bipaths(start: T, path: List[T]): List[List[T]] = {
      if (start.equals(end)) List(path)
      else neighbors(start).filter(nb => !path.contains(nb)).toList.flatMap(nb => bipaths(nb, nb :: path))
    }

    bipaths(start, List(start)).map(_.reverse)
  }

  /**
   * Find a path that contains all nodes in (nodes).
   */
  def bipaths(nodes: Set[T]): Set[List[T]] = {
    def bipaths(start: T, path: List[T]): List[List[T]] = {
      if (nodes.forall(path.contains(_))) List(path)
      else neighbors(start).filter(nb => !path.contains(nb)).toList.flatMap(nb => bipaths(nb, nb :: path))
    }

    nodes.flatMap(start => bipaths(start, List(start)).map(_.reverse))
  }

  def contents(vertex: T)(implicit ord: Ordering[T]): List[String] = inferiors(vertex).toList.sorted.map(vertex => vertex.toString)

  def print() {
    def print(node: T, indent: Int) {
      println(" " * indent + node)
      outgoing(node).foreach { edge => print(edge.dest, indent + 2) }
    }

    val start = nodes.find(node => incoming(node).isEmpty).get
    print(start, 0)
  }
}

object Graph {
  def vertices[T](edges: Iterable[Edge[T]]) = {
    edges.flatMap(edge => List(edge.source, edge.dest)).toSet
  }

  class Edge[T] (
      val source: T,
      val dest: T,
      val label: String
    ) {
    def vertices = List(source, dest)
    override def toString = label+"("+source+", "+dest+")"
  }
}

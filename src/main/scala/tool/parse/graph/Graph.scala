package edu.washington.cs.knowitall
package tool
package parse
package graph

import scala.collection._

class Edge[V] (
    val source: V,
    val dest: V,
    val label: String
  ) {
  def vertices = List(source, dest)
}

class Graph[V] (
    val vertices: Iterable[V], 
    val edges: Iterable[Edge[V]]
  ) {

  type G = Graph[V]
  type E = Edge[V]

  // fields

  val _outgoing = new mutable.HashMap[V, mutable.Set[E]]() 
    with mutable.MultiMap[V, E]
  val _incoming = new mutable.HashMap[V, mutable.Set[E]]() 
    with mutable.MultiMap[V, E]
  val nodes = edges.map(dep => dep.source).toSet union edges.map(dep => dep.dest).toSet
  edges.foreach { dep => _outgoing.addBinding(dep.source, dep) }
  edges.foreach { dep => _incoming.addBinding(dep.dest, dep) }

  // constructors
  
  def this(edges: Iterable[Edge[V]]) =
    this(Graph.vertices(edges), edges)
  
  /* Expand a set of `Vertex`s to all neighbors along immediate edges
   * that satisfy the supplied predicate. 
   * 
   * @param  deps  the seed `Vertex`s
   * @param  pred  the predicate edges must match to be expanded upon
   * @return  the  the set of `Vertex`s in the expansion
   */
  def expand(deps: Set[V], pred: DirectedEdge[V]=>Boolean) = {
    deps.flatMap { node => neighbors(node, pred) + node }
  }
  
  /* Iteratively expand the neighbors of a `Vertex` to all
   * neighbors along an edge that satisfy the supplied predicate.
   * 
   * @param  dep  the seed `Vertex`
   * @param  pred  the predicate edges must match to be expanded upon
   * @return  the set of `Vertex`s in the expansion
   */
  def connected(dep: V, pred: DirectedEdge[V]=>Boolean): Set[V] = {
    var nodes = Set(dep)
    var last: Set[V] = Set.empty
    while (nodes.size > last.size) {
      last = nodes
      nodes = expand(nodes, pred)
    }
    
    nodes
  }

  def collapse(collapsable: E => Boolean)(implicit merge: Traversable[V] => V): G = {
    // find nn edges
    val (nndeps, otherdeps) = edges.partition(collapsable)

    // collapse edges by building a map from collapsed nodes
    // to collections of joined nodes
    var map: Map[V, mutable.Set[V]] = Map()
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
        val set = new mutable.HashSet[V]()
        set += dep.source
        set += dep.dest
        map += dep.dest -> set
        map += dep.source -> set
      }
    }
    
    collapseGroups(map.values)(merge)
  }

  def collapse(set: Set[V])(implicit merge: Traversable[V] => V) = collapseGroups(Iterable(set))
  
  def collapseGroups(groups: Iterable[Set[V]])(implicit merge: Traversable[V] => V) = {
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

  def outgoing(node: V): immutable.Set[E] =
    _outgoing.getOrElse(node, mutable.HashSet.empty).toSet

  def incoming(node: V): immutable.Set[E] =
    _incoming.getOrElse(node, mutable.HashSet.empty).toSet

  def edges(node: V): immutable.Set[E] = (outgoing(node).toSet) union (incoming(node).toSet)

  def dedges(node: V): immutable.Set[DirectedEdge[V]] = 
    outgoing(node).map(new DownEdge(_): DirectedEdge[V]).union(
      incoming(node).map(new UpEdge(_): DirectedEdge[V])).toSet
    
  def neighbors(v: V, pred: DirectedEdge[V]=>Boolean): Set[V] =
    dedges(v).filter(pred).map { _ match { 
      case out: DownEdge[_] => out.end
      case in: UpEdge[_] => in.start
    }}

  def neighbors(v: V): Set[V] = predecessors(v) union successors(v)

  def predecessors(v: V) = incoming(v).map(edge => edge.source)

  def successors(v: V) = outgoing(v).map(edge => edge.dest)

  /* Iteratively expand a vertex to all nodes beneath it. 
   * 
   * @param  vertex  the seed vertex
   * @return  the set of vertices beneath `vertex`
   */
  def inferiors(v: V, cond: E=>Boolean = (x=>true)): List[V] =
    v :: outgoing(v).filter(cond).map(edge => inferiors(edge.dest)).toList.flatten

  /* Iteratively expand a vertex to all nodes above it. 
   * 
   * @param  vertex  the seed vertex 
   * @return  the set of vertices beneath `vertex`
   */
  def superiors(v: V, cond: E=>Boolean = (x=>true)): List[V] =
    v :: outgoing(v).filter(cond).map(edge => inferiors(edge.source)).toList.flatten

  /* number of out-edges bordering v */
  def outdegree(v: V) = outgoing(v).size
  /* number of in-edges bordering v */
  def indegree(v: V) = incoming(v).size
  /* number of edges bordering v */
  def degree(v: V) = indegree(v) + outdegree(v)

  private def toBipath(nodes: List[V]) = {
    def toEdgeBipath(nodes: List[V]): List[List[DirectedEdge[V]]] = nodes match {
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

  def edgeBipaths(start: V, end: V): List[Bipath[V]] = {
    val nodePaths = bipaths(start, end)
    nodePaths.flatMap(np => toBipath(np))
  }

  def edgeBipaths(nodes: Set[V]): Set[Bipath[V]] = {
    val nodePaths = bipaths(nodes)
    nodePaths.flatMap(np => toBipath(np))
  }

  /**
   * Find a path from node (start) to node (end).
   */
  def bipaths(start: V, end: V): List[List[V]] = {
    def bipaths(start: V, path: List[V]): List[List[V]] = {
      if (start.equals(end)) List(path)
      else neighbors(start).filter(nb => !path.contains(nb)).toList.flatMap(nb => bipaths(nb, nb :: path))
    }

    bipaths(start, List(start)).map(_.reverse)
  }

  /**
   * Find a path that contains all nodes in (nodes).
   */
  def bipaths(nodes: Set[V]): Set[List[V]] = {
    def bipaths(start: V, path: List[V]): List[List[V]] = {
      if (nodes.forall(path.contains(_))) List(path)
      else neighbors(start).filter(nb => !path.contains(nb)).toList.flatMap(nb => bipaths(nb, nb :: path))
    }

    nodes.flatMap(start => bipaths(start, List(start)).map(_.reverse))
  }

  def contents(vertex: V)(implicit ord: Ordering[V]): List[String] = inferiors(vertex).sorted.map(vertex => vertex.toString)

  def print() {
    def print(node: V, indent: Int) {
      println(" " * indent + node)
      outgoing(node).foreach { edge => print(edge.dest, indent + 2) }
    }

    val start = nodes.find(node => incoming(node).isEmpty).get
    print(start, 0)
  }
}

object Graph {
  def vertices[V](edges: Iterable[Edge[V]]) = {
    edges.flatMap(edge => List(edge.source, edge.dest)).toSet
  }
}

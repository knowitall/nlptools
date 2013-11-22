package edu.knowitall.tool.parse.graph

import edu.knowitall.collection.immutable.graph.Graph
import edu.knowitall.collection.immutable.graph.Graph.Edge
import edu.knowitall.tool.postag.PostaggedToken

class JoinedDependencyGraph(vertices: Set[JoinedDependencyNode], edges: Set[Edge[JoinedDependencyNode]])
  extends Graph[JoinedDependencyNode](vertices, edges) {

  def this(edges: Iterable[Edge[JoinedDependencyNode]]) =
    this(edges.flatMap(_.vertices).toSet, edges.toSet)
  
  val nodes = vertices
  val dependencies = edges

  /** Join NNPs connected by "of" into a single node. */
  def collapseNNPOf(tokens: Seq[PostaggedToken]) = {
    def nodeTokens(node: JoinedDependencyNode) = {
      node.ids map tokens.apply
    }

    def pred(edge: Edge[JoinedDependencyNode]) = {
      val sourcePostagsIt = nodeTokens(edge.source).iterator.map(_.postag)
      val destPostagsIt = nodeTokens(edge.dest).iterator.map(_.postag)
      (edge.source.span distance edge.dest.span) == 2 &&
      edge.label.equals("prep_of") && (sourcePostagsIt contains "NNP") && (destPostagsIt contains "NNP")
    }

    def merge(nodes: Traversable[JoinedDependencyNode]) = {
      if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
      val sorted = nodes.toList.sortBy(_.ids.head)
      sorted.sliding(2).foreach { l => 
        require((l.head.span distance l.last.span) == 2, 
            "two nodes to merge don't have a distance of 2 (distance is "+(l.head.span distance l.last.span)+"): " + l.mkString(", "))
      }
      val strings = (for(i <- sorted.iterator.map(_.string); p <- List("of", i)) yield p).drop(1).toSeq
      val ids = sorted.flatMap(_.ids).sorted
      new JoinedDependencyNode(ids, strings)
    }

    this.collapse(pred(_))(merge)
  }

  /**
    * Find components that are connected by the predicate.
    * Then, split components into subcomponents in which
    * all vertices correspond to adjacent words in the
    * source sentence.
    */
  def adjacentComponents(pred: Edge[JoinedDependencyNode]=>Boolean): Set[Set[JoinedDependencyNode]] = {
    def splitByAdjacency(nodes: List[JoinedDependencyNode]): List[List[JoinedDependencyNode]] = {
      def rec(nodes: List[JoinedDependencyNode], result: List[JoinedDependencyNode]): List[List[JoinedDependencyNode]] = nodes match {
        case x :: Nil => (x :: result) :: Nil
        case x :: y :: xs => if (x.span borders y.span) rec(y :: xs, x :: result) else (x :: result) :: rec(y :: xs, Nil)
        case Nil => Nil
      }

      rec(nodes, Nil)
    }

    val groups: Set[Set[JoinedDependencyNode]] = (for (dep <- this.edges; if pred(dep)) yield {
      this.connected(dep.source, dedge=>pred(dedge.edge)).toSet
    })(scala.collection.breakOut)


    (for {
      // for each connect nn component
      group <- groups
      // split the component by POS tag
      nodes = group.toList.sortBy(_.ids.head)
      part <- splitByAdjacency(nodes)
      if part.size > 1
    } yield(part.toSet))(scala.collection.breakOut)
  }

//  def collapseAdjacentGroups(pred: Edge[JoinedDependencyNode]=>Boolean)
//      (implicit merge: Traversable[JoinedDependencyNode]=>JoinedDependencyNode) = {
//    val components = adjacentComponents(edge => pred(edge))
//    val graph = this.graph.collapseGroups(components)(merge)
//    new DependencyGraph(this.tokens, this.dependencies, graph)
//  }
//
//  def collapseNounGroups(dividors: List[String] = List.empty) = {
//    val lowerCaseDividors = dividors.map(_.toLowerCase)
//
//    def pred(edge: Edge[JoinedDependencyNode]) = edge.label == "nn"
//    val groups = adjacentComponents(pred)
//
//    def splitByDividor(nodes: List[JoinedDependencyNode]): List[List[JoinedDependencyNode]] = nodes match {
//      case x :: xs if lowerCaseDividors.contains(x.text.toLowerCase) => List(x) :: splitByDividor(xs)
//      case x :: xs =>
//        val (part, rest) = nodes.span(node => !lowerCaseDividors.contains(node.text.toLowerCase))
//        part :: splitByDividor(rest)
//      case Nil => Nil
//    }
//
//    // segment ordered dependency nodes by POS tag
//    def postagEqual(a: String, b: String) = a == b || a.startsWith("NNP") && b.startsWith("NNP")
//    def splitByPos(nodes: List[JoinedDependencyNode]): List[List[JoinedDependencyNode]] = nodes match {
//      case x :: xs => nodes.takeWhile(node => postagEqual(node.postag, x.postag)) ::
//        splitByPos(nodes.dropWhile(node => postagEqual(node.postag, x.postag)))
//      case Nil => Nil
//    }
//
//    val groupsToCollapse: Set[Set[JoinedDependencyNode]] = (for {
//      // for each connect nn component
//      group <- groups
//      // split the component by POS tag
//      nodes = group.toList.sorted
//      dividorSplit <- splitByDividor(nodes)
//      part <- splitByPos(dividorSplit)
//      if part.size > 1
//    } yield(part.toSet))(scala.collection.breakOut)
//
//    new DependencyGraph(this.tokens, this.dependencies, graph.collapseGroups(groupsToCollapse))
//  }

  def directedAdjacentCollapse(labels: Set[String]): JoinedDependencyGraph = {
    def pred(edge: Edge[JoinedDependencyNode]) = labels.contains(edge.label)

    // If we get a component that is not connected, remove it from consideration.
    // It is often a mistake due to a strange parse.  It may also be an unusual edge.
    val components = adjacentComponents(pred) filter (this.areConnected)
    val graph = this.collapseGroups(components)(JoinedDependencyNode.directedMerge(this))
    new JoinedDependencyGraph(graph.vertices, graph.edges)
  }

  def collapseWeakLeaves =
    directedAdjacentCollapse(Set("neg", "det", "aux", "amod", "num", "quantmod", "advmod"))
}
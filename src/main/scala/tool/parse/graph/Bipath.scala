package edu.washington.cs.knowitall
package tool
package parse
package graph

class Bipath[V <: Vertex](val path: List[DirectedEdge[V]]) {
  def edges = path.foldRight[Set[Edge[V]]](Set()) { case (item, set) => set + item.edge }
  def nodes = path.head.start :: path.map(_.end)
  def start = path.head.start
  def collapse(pred: Edge[V]=>Boolean, merge: (V, V) => V) = {
    if (path.forall(dep => pred(dep.edge))) {
      this
    } else {
      val array = path.toArray
      for (i <- array.indices) {
        val current = array(i)
        if (pred(current.edge)) {
          // TODO: sorted
          val merged = merge(current.start, current.end)
          if (current.isInstanceOf[UpEdge[V]]) {
            if (array.indices contains (i + 1)) {
              array(i + 1) = array(i + 1).switchStart(merged)
            }

            if (array.indices contains (i - 1)) {
              array(i - 1) = array(i - 1).switchEnd(merged)
            }
          } else if (current.isInstanceOf[DownEdge[V]]) {
            if (array.indices contains (i + 1)) {
              array(i + 1).switchStart(merged)
            }

            if (array.indices contains (i - 1)) {
              array(i - 1) = array(i - 1).switchEnd(merged)
            }
          }
        }
      }

      new Bipath(array.filter(dep => !pred(dep.edge)).toList)
    }
  }
  /*
  def collapseNN = {
    collapse(_.label.equals("nn"))
  }
  def collapseHeuristic = {
    collapse(
      edge => 
        edge.label == "nn" && edge.source.pos.equals(edge.dest.pos) ||
        edge.label == "prep_of" && edge.source.pos.equals("NNP") && edge.dest.pos.equals("NNP"))
  }
  */
  override def toString = "[" + path.mkString(", ") + "]";
}
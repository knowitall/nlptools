package edu.washington.cs.knowitall
package tool
package parse
package graph

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
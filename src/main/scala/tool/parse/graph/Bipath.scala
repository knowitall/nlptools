package edu.washington.cs.knowitall
package tool
package parse
package graph

import Graph._

class Bipath[T](val path: List[DirectedEdge[T]]) {
  // extend Object
  override def toString = "[" + path.mkString(", ") + "]";
  def canEqual(that: Any) = that.isInstanceOf[Bipath[_]]
  override def equals(that: Any) = that match {
    case that: Bipath[_] => (that canEqual this) && that.path == this.path
    case _ => false
  }

  def edges = path.foldRight[Set[Edge[T]]](Set()) { case (item, set) => set + item.edge }
  def nodes = path.head.start :: path.map(_.end)
  def start = path.head.start
  def collapse(pred: Edge[T]=>Boolean, merge: (T, T) => T) = {
    if (path.forall(dep => pred(dep.edge))) {
      this
    } else {
      val array = path.toArray
      for (i <- array.indices) {
        val current = array(i)
        if (pred(current.edge)) {
          // TODO: sorted
          val merged = merge(current.start, current.end)
          if (current.isInstanceOf[UpEdge[_]]) {
            if (array.indices contains (i + 1)) {
              array(i + 1) = array(i + 1).switchStart(merged)
            }

            if (array.indices contains (i - 1)) {
              array(i - 1) = array(i - 1).switchEnd(merged)
            }
          } else if (current.isInstanceOf[DownEdge[_]]) {
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
}

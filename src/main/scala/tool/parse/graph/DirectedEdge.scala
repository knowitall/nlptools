package edu.washington.cs.knowitall
package tool
package parse
package graph

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

sealed abstract class DirectedEdge[V <: Vertex](val edge: Edge[V]) {
  def start: V
  def end: V
  def dir: Direction
  def switchStart(newStart: V): DirectedEdge[V]
  def switchEnd(newEnd: V): DirectedEdge[V]
  def flip: DirectedEdge[V]
  
  override def toString() = edge.toString
  def canEqual(that: Any): Boolean
  override def equals(that: Any) = that match {
    case that: DirectedEdge[V] => (that canEqual this) && that.edge == this.edge
    case _ => false
  }
}

class UpEdge[V <: Vertex](edge: Edge[V]) extends DirectedEdge[V](edge) {
  def start = edge.dest
  def end = edge.source
  def dir = Direction.Up
  def switchStart(newStart: V) =
    new UpEdge(new Edge[V](edge.source, newStart, edge.label))
  def switchEnd(newEnd: V) =
    new UpEdge(new Edge[V](newEnd, edge.dest, edge.label))
  def flip = new DownEdge[V](edge)
  
  override def toString() = "Up(" + super.toString + ")"
  override def canEqual(that: Any) = that.isInstanceOf[UpEdge[V]]
  override def hashCode() = (edge.hashCode + 2) * 37
}

class DownEdge[V <: Vertex](edge: Edge[V]) extends DirectedEdge[V](edge) {
  def start = edge.source
  def end = edge.dest
  def dir = Direction.Down
  def switchStart(newStart: V) =
    new DownEdge(new Edge[V](newStart, edge.dest, edge.label))
  def switchEnd(newEnd: V) =
    new DownEdge(new Edge[V](edge.source, newEnd, edge.label))
  def flip = new UpEdge[V](edge)
  
  override def toString() = "Down(" + super.toString + ")"
  override def canEqual(that: Any) = that.isInstanceOf[DownEdge[V]]
  override def hashCode() = (edge.hashCode + 1) * 37
}
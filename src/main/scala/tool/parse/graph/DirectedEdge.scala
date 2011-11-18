package edu.washington.cs.knowitall
package tool
package parse
package graph

import Graph._

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

sealed abstract class DirectedEdge[V](val edge: Edge[V]) {
  def start: V
  def end: V
  def dir: Direction
  def switchStart(newStart: V): DirectedEdge[V]
  def switchEnd(newEnd: V): DirectedEdge[V]
  def flip: DirectedEdge[V]
  
  override def toString() = edge.toString
  def canEqual(that: Any): Boolean
  override def equals(that: Any) = that match {
    case that: DirectedEdge[_] => (that canEqual this) && that.edge == this.edge
    case _ => false
  }
}

class UpEdge[V](edge: Edge[V]) extends DirectedEdge[V](edge) {
  def start = edge.dest
  def end = edge.source
  def dir = Direction.Up
  def switchStart(newStart: V) =
    new UpEdge(new Edge[V](edge.source, newStart, edge.label))
  def switchEnd(newEnd: V) =
    new UpEdge(new Edge[V](newEnd, edge.dest, edge.label))
  def flip = new DownEdge[V](edge)
  
  override def toString() = "Up(" + super.toString + ")"
  override def canEqual(that: Any) = that.isInstanceOf[UpEdge[_]]
  override def hashCode() = (edge.hashCode + 2) * 37
}
object UpEdge {
  def unapply[V](dedge: DownEdge[V]) = Some(dedge.edge)
}

class DownEdge[V](edge: Edge[V]) extends DirectedEdge[V](edge) {
  def start = edge.source
  def end = edge.dest
  def dir = Direction.Down
  def switchStart(newStart: V) =
    new DownEdge(new Edge[V](newStart, edge.dest, edge.label))
  def switchEnd(newEnd: V) =
    new DownEdge(new Edge[V](edge.source, newEnd, edge.label))
  def flip = new UpEdge[V](edge)
  
  override def toString() = "Down(" + super.toString + ")"
  override def canEqual(that: Any) = that.isInstanceOf[DownEdge[_]]
  override def hashCode() = (edge.hashCode + 1) * 37
}
object DownEdge {
  def unapply[V](dedge: DownEdge[V]) = Some(dedge.edge)
}

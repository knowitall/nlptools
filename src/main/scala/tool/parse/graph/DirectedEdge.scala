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

sealed abstract class DirectedEdge[T](val edge: Edge[T]) {
  def start: T
  def end: T
  def dir: Direction
  def switchStart(newStart: T): DirectedEdge[T]
  def switchEnd(newEnd: T): DirectedEdge[T]
  def flip: DirectedEdge[T]
  
  override def toString() = edge.toString
  def canEqual(that: Any): Boolean
  override def equals(that: Any) = that match {
    case that: DirectedEdge[_] => (that canEqual this) && that.edge == this.edge
    case _ => false
  }
}

class UpEdge[T](edge: Edge[T]) extends DirectedEdge[T](edge) {
  def start = edge.dest
  def end = edge.source
  def dir = Direction.Up
  def switchStart(newStart: T) =
    new UpEdge(new Edge[T](edge.source, newStart, edge.label))
  def switchEnd(newEnd: T) =
    new UpEdge(new Edge[T](newEnd, edge.dest, edge.label))
  def flip = new DownEdge[T](edge)
  
  override def toString() = "Up(" + super.toString + ")"
  override def canEqual(that: Any) = that.isInstanceOf[UpEdge[_]]
  override def hashCode() = (edge.hashCode + 2) * 37
}
object UpEdge {
  def unapply[T](dedge: DownEdge[T]) = Some(dedge.edge)
}

class DownEdge[T](edge: Edge[T]) extends DirectedEdge[T](edge) {
  def start = edge.source
  def end = edge.dest
  def dir = Direction.Down
  def switchStart(newStart: T) =
    new DownEdge(new Edge[T](newStart, edge.dest, edge.label))
  def switchEnd(newEnd: T) =
    new DownEdge(new Edge[T](edge.source, newEnd, edge.label))
  def flip = new UpEdge[T](edge)
  
  override def toString() = "Down(" + super.toString + ")"
  override def canEqual(that: Any) = that.isInstanceOf[DownEdge[_]]
  override def hashCode() = (edge.hashCode + 1) * 37
}
object DownEdge {
  def unapply[T](dedge: DownEdge[T]) = Some(dedge.edge)
}

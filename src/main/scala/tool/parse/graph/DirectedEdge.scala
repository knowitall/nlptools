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

sealed abstract class DirectedEdge(val edge: Dependency) {
  def start: DependencyNode
  def end: DependencyNode
  def dir: Direction
  def switchStart(newStart: DependencyNode): DirectedEdge
  def switchEnd(newEnd: DependencyNode): DirectedEdge
  def flip: DirectedEdge
  
  override def toString() = edge.toString
  def canEqual(that: Any): Boolean
  override def equals(that: Any) = that match {
    case that: DirectedEdge => (that canEqual this) && that.edge == this.edge
    case _ => false
  }
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
  override def canEqual(that: Any) = that.isInstanceOf[UpEdge]
  override def hashCode() = (edge.hashCode + 2) * 37
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
  override def canEqual(that: Any) = that.isInstanceOf[DownEdge]
  override def hashCode() = (edge.hashCode + 1) * 37
}
package edu.washington.cs.knowitall
package tool
package parse
package graph

import stem.Stemmer

object DependencyNode {
  def fromLists(tokens: Iterable[String], pos: Iterable[String]) =
    (tokens zip pos).zipWithIndex.map { case ((token, pos), index) => new DependencyNode(token, pos, index) }.toArray

  def merge(nodes: List[DependencyNode]) = {
    if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
    val sorted = nodes.sortBy(_.index).view
    new DependencyNode(sorted.map(_.text).mkString(" "), 
        if (nodes.forall(_.pos.equals(nodes.head.pos))) 
          nodes.head.pos
        else
          sorted.map(_.pos).mkString(" "), sorted.head.index)
  }

  // TODO: rewrite using foldr
  def nodes(dependencies: Iterable[Dependency]) = {
    var ns: Set[DependencyNode] = Set()
    for (dep <- dependencies) {
      ns += dep.source
      ns += dep.dest
    }
    ns
  }
  
  def deserialize(string: String) = {
    val Array(text, pos, index) = string.split("_")
    new DependencyNode(text, pos, index.toInt)
  }
}

class DependencyNode(val text: String, val pos: String, val index: Int) extends Ordered[DependencyNode] {
  //override def toString() = this.text + "_" + index
  override def compare(other: DependencyNode) = this.index - other.index
  override def toString() = this.text
  override def equals(other: Any) =
    other != null && other.isInstanceOf[DependencyNode] &&
      other.asInstanceOf[DependencyNode].text.equals(text) &&
      other.asInstanceOf[DependencyNode].index == index
  override def hashCode() = this.text.hashCode * 37 + index.hashCode

  def normalize(stemmer: Stemmer) = new DependencyNode(stemmer.stem(text), pos, index)
  def serialize = text.replaceAll("[_(),]", "") + "_" + pos + "_" + index;
}

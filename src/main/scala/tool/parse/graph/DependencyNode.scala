package edu.washington.cs.knowitall
package tool
package parse
package graph

import stem.Stemmer

object DependencyNode {
  def fromLists(tokens: Iterable[String], postag: Iterable[String]) =
    (tokens zip postag).zipWithIndex.map { case ((token, pos), index) => new DependencyNode(token, pos, index) }.toArray

  implicit def merge(nodes: Traversable[DependencyNode]) = {
    if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
    val sorted = nodes.toList.sortBy(_.index).view
    new DependencyNode(sorted.map(_.text).mkString(" "), 
        if (nodes.forall(_.postag.equals(nodes.head.postag))) 
          nodes.head.postag
        else
          sorted.map(_.postag).mkString(" "), sorted.head.index)
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
    val Array(text, postag, index) = string.split("_")
    new DependencyNode(text, postag, index.toInt)
  }
}

class DependencyNode(val text: String, val postag: String, val index: Int) extends Ordered[DependencyNode] {
  override def compare(that: DependencyNode) = this.index - that.index
  override def toString() = this.text
  override def equals(other: Any) =
    other != null && other.isInstanceOf[DependencyNode] &&
      other.asInstanceOf[DependencyNode].text.equals(text) &&
      other.asInstanceOf[DependencyNode].index == index
  override def hashCode() = this.text.hashCode * 37 + index.hashCode

  def lemmatize(stemmer: Stemmer) = new DependencyNode(stemmer.lemmatize(text), postag, index)
  def serialize = text.replaceAll("[_(),]", "") + "_" + postag + "_" + index;
}

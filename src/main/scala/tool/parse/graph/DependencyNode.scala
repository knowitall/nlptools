package edu.washington.cs.knowitall
package tool
package parse
package graph

import stem.Stemmer
import scala.collection.immutable.SortedSet

object DependencyNode {
  def fromLists(tokens: Iterable[String], postag: Iterable[String]) =
    (tokens zip postag).zipWithIndex.map { case ((token, pos), index) => new DependencyNode(token, pos, index) }.toArray

  implicit def merge(nodes: Traversable[DependencyNode]) = {
    if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
    val sorted = nodes.toList.sorted
    val text = sorted.map(_.text).mkString(" ")
    val postag = if (nodes.forall(_.postag.equals(nodes.head.postag))) 
          nodes.head.postag
        else
          sorted.map(_.postag).mkString(" ")
    val indices = sorted.map(_.indices).reduce(_ ++ _)
    new DependencyNode(text, postag, indices)
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

class DependencyNode(val text: String, val postag: String, val indices: SortedSet[Int]) extends Ordered[DependencyNode] {
  def this(text: String, postag: String, index: Int) = 
    this(text, postag, SortedSet(index))
  
  override def compare(that: DependencyNode) = {
	if (this.indices.exists(that.indices.contains)) throw new IllegalStateException("overlapping ranges cannot be compared")
	else this.indices.max.compare(that.indices.max)
  }
  override def toString() = this.text
  def toFullString = this.text + "_" + this.postag + "_" + this.indices.mkString("_")
  
  def canEqual(that: Any) = that.isInstanceOf[DependencyNode]
  override def equals(that: Any) = that match {
    case that: DependencyNode => that.text.equals(this.text) &&
      that.postag.equals(this.postag) &&
      that.indices.equals(this.indices)
    case _ => false
  }
  override def hashCode() = this.text.hashCode * 37 + this.postag.hashCode * 37 + this.indices.hashCode

  def lemmatize(stemmer: Stemmer) = new DependencyNode(stemmer.lemmatize(text), postag, indices)
  def serialize = {
    if (indices.size > 1) throw new IllegalStateException("cannot serialize node spanning multiple indices")
    text.replaceAll("[_(),]", "") + "_" + postag + "_" + indices.head;
  }
}

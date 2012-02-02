package edu.washington.cs.knowitall
package tool
package parse
package graph

import scala.collection.immutable.SortedSet
import collection.immutable.Interval
import tool.stem.Stemmer

object DependencyNode {
  def fromLists(tokens: Iterable[String], postag: Iterable[String]) =
    (tokens zip postag).zipWithIndex.map { case ((token, pos), index) => new DependencyNode(token, pos, index) }.toArray

  implicit def merge(nodes: Traversable[DependencyNode]) = {
    if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
    val sorted = nodes.toList.sorted
    val text = sorted.map(_.text).mkString(" ")
    val postag = 
      // if they all have the same postag, use that
      if (nodes.forall(_.postag.equals(nodes.head.postag))) 
        nodes.head.postag
      // otherwise use the first postag
      else
        sorted.map(_.postag).head

    // union the intervals, or throw a more informative exception if they are
    // not adjacent
    val indices = try {
      Interval.union(sorted.map(_.indices))
    }
    catch {
      case e: IllegalArgumentException => 
        throw new IllegalArgumentException("A set of non-adjacent intervals cannot be merged: " + e.toString)
    }

    new DependencyNode(text, postag, indices)
  }
  
  def deserialize(string: String) = {
    val Array(text, postag, index) = string.split("_")
    new DependencyNode(text, postag, index.toInt)
  }
}

/*
 * A representation for a node in the graph of dependencies.  A node
 * represents one or more adjacent tokens in the source sentence. */
class DependencyNode(val text: String, val postag: String, val indices: Interval) extends Ordered[DependencyNode] {
  require(text != null)
  require(postag != null)
  
  /* create a node with a single index */
  def this(text: String, postag: String, index: Int) = 
    this(text, postag, Interval.singleton(index))
  
  // extend Ordered[DependencyNode]
  override def compare(that: DependencyNode) = {
    if (this == that) 0
    else if (this.indices intersects that.indices) 
      throw new IllegalStateException("intersecting intervals cannot be compared: " + this.toFullString + " and " + that.toFullString)
    else this.indices.max.compare(that.indices.max)
  }

  // extend Object
  override def toString() = toFullString //this.text
  def canEqual(that: Any) = that.isInstanceOf[DependencyNode]
  override def equals(that: Any) = that match {
    case that: DependencyNode => that.text.equals(this.text) &&
      that.postag.equals(this.postag) &&
      that.indices.equals(this.indices)
    case _ => false
  }
  override def hashCode() = this.text.hashCode * 37 + this.postag.hashCode * 37 + this.indices.hashCode

  def toFullString = this.text + "_" + this.postag + "_" + this.indices.mkString("_")

  def isProperNoun = postag == "NNP" || postag == "NNPS"
  def isCommonNoun = postag == "NN" || postag == "NNS"
  def isNoun = isProperNoun || isCommonNoun
  def isVerb = postag.startsWith("VB")
  def isAdjective = postag == "JJ" || postag == "JJS"

  def lemmatize(stemmer: Stemmer) = new DependencyNode(stemmer.lemmatize(text), postag, indices)
  def serialize = {
    if (indices.length > 1) throw new IllegalStateException("cannot serialize node spanning multiple indices")
    text.replaceAll("[_(),]", "") + "_" + postag + "_" + indices.start;
  }
}

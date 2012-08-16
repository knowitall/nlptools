package edu.washington.cs.knowitall
package tool
package parse
package graph

import collection.immutable.graph.Graph
import collection.immutable.graph.Graph._
import scala.collection.immutable.SortedSet
import collection.immutable.Interval
import tool.stem.{ Stemmer, IdentityStemmer }
import tool.postag.PostaggedToken

/** A representation for a node in the graph of dependencies.  A node
  * represents one or more adjacent tokens in the source sentence.
  */
class DependencyNode(string: String, postag: String, val indices: Interval, offset: Int) extends PostaggedToken(postag, string, offset) with Ordered[DependencyNode] {
  require(string != null)
  require(postag != null)

  /* create a node with a single index */
  def this(text: String, postag: String, index: Int, offset: Int) =
    this(text, postag, Interval.singleton(index), offset)
  
  def this(postagged: PostaggedToken, indices: Interval) = 
    this(postagged.string, postagged.postag, indices, postagged.offset)

  def text = string

  // extend Ordered[DependencyNode]
  override def compare(that: DependencyNode) = {
    if (this == that) 0
    else if (this.indices intersects that.indices)
      throw new IllegalStateException("intersecting intervals cannot be compared: " + this.toFullString + " and " + that.toFullString)
    else this.indices.compare(that.indices)
  }

  // extend Object
  override def toString() = toFullString
  def canEqual(that: Any) = that.isInstanceOf[DependencyNode]
  override def equals(that: Any) = that match {
    case that: DependencyNode => that.text.equals(this.text) &&
      that.postag.equals(this.postag) &&
      that.indices.equals(this.indices)
    case _ => false
  }
  override def hashCode() = this.text.hashCode * 37 + this.postag.hashCode * 37 + this.indices.hashCode

  private var plemma: String = null
  def lemma(implicit stemmer: Stemmer) = {
    if (plemma == null) {
      plemma = stemmer.lemmatize(text)
    }
    plemma
  }

  def toFullString = this.text + "_" + this.postag + "_" + this.indices.mkString("_")

  def lemmatize(stemmer: Stemmer) = new DependencyNode(stemmer.lemmatize(text), postag, indices, offset)
  def serialize = {
    if (indices.length > 1) throw new IllegalStateException("cannot serialize node spanning multiple indices")
    text.replaceAll("[[_()][^\\p{Graph}]]", "") + "_" + postag + "_" + indices.start + "_" + offset;
  }
}

object DependencyNode {
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
      case e =>
        throw new IllegalArgumentException("A set of non-adjacent intervals cannot be merged: " + e.toString)
    }

    new DependencyNode(text, postag, indices, sorted.iterator.map(_.offset).min)
  }

  /**
    * Merge nodes, keeping the postag of the superior node of the set.
    *
    * @throws  IllegalArgumentException  there is no superior of the set
    * @return  the superior node of the set
    */
  implicit def directedMerge(graph: Graph[DependencyNode])(nodes: Traversable[DependencyNode]) = {
    if (nodes.isEmpty) throw new IllegalArgumentException("argument nodes empty")
    val sorted = nodes.toList.sorted
    val text = sorted.map(_.text).mkString(" ")
    val postag = graph.superior(nodes.toSet).postag

    // union the intervals, or throw a more informative exception if they are
    // not adjacent
    val indices = try {
      Interval.union(sorted.map(_.indices))
    }
    catch {
      case e =>
        throw new IllegalArgumentException("A set of non-adjacent intervals cannot be merged: " + e.toString)
    }

    new DependencyNode(text, postag, indices, sorted.iterator.map(_.offset).min)
  }

  def deserialize(string: String) = {
    val Array(text, postag, index, offset) = try (string.split("_"))
    catch {
      case e => throw new SerializationException("could not deserialize dependency node: " + string, e);
    }

    new DependencyNode(text, postag, index.toInt, offset.toInt)
  }

  class SerializationException(message: String, cause: Throwable)
  extends RuntimeException(message, cause)
}

package edu.washington.cs.knowitall
package tool
package parse
package graph

import collection.immutable.graph.Graph._
import stem.Stemmer
import scala.util.matching.Regex
import scala.collection.immutable.SortedSet

/*
 * A representation for an edge in the graph of dependencies. */
class Dependency(
    source: DependencyNode, 
    dest: DependencyNode, 
    label: String) 
extends Edge[DependencyNode](source, dest, label) 
with Ordered[Dependency] {
  require(source != null)
  require(dest != null)
  require(label != null)
  
  // extend Object
  override def toString() = this.label + "(" + this.source + " -> " + this.dest + ")"
  override def equals(that: Any) = that match {
    case that: Dependency => this.label == that.label && this.source == that.source && this.dest == that.dest
    case _ => false
  }
  override def hashCode() = 37 * (this.source.hashCode + this.dest.hashCode * 37) + label.hashCode
  
  // extend Ordered
  def compare(that: Dependency) = {
    def tuplize(dep: Dependency) =
      (dep.source.indices.start, dep.dest.indices.start, dep.label)
    implicitly[Ordering[(Int, Int, String)]].compare(tuplize(this), tuplize(that))
  }

  def nodes = Set(source, dest)
  def otherNode(node: DependencyNode) = 
    if (source == dest) throw new IllegalStateException("source == dest")
    else if (source == node) dest
    else source

  def mapNodes(f: DependencyNode=>DependencyNode) = {
    new Dependency(f(source), f(dest), label)
  }

  def lemmatize(stemmer: Stemmer) = new Dependency(source.lemmatize(stemmer), dest.lemmatize(stemmer), label)
  def serialize = label + "(" + source.serialize + ", " + dest.serialize + ")"
}

object Dependency {
  val Serialized = new Regex("""(\p{Graph}+)\(\s*(\p{Graph}*?_\p{Graph}*?_\p{Graph}*?),\s*(\p{Graph}*?_\p{Graph}*?_\p{Graph}*?)\s*\)""")
  def deserialize(string: String) = try {
    val Serialized(label, source, dest) = string
    new Dependency(
        DependencyNode.deserialize(source), 
        DependencyNode.deserialize(dest), 
        label)
  }
  catch {
    case e => throw new Dependency.SerializationException("could not deserialize dependency: " + string, e)
  }
  
  
  class SerializationException(message: String, cause: Throwable) 
  extends RuntimeException(message, cause)
}


object Dependencies {
  def serialize(deps: Iterable[Dependency]) = (deps.iterator).map(_.serialize).mkString("; ")
  def deserialize(string: String): SortedSet[Dependency] = string.split("""\s*(?:;|\n)\s*""").
      map(Dependency.deserialize(_))(scala.collection.breakOut);
}

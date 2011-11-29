package edu.washington.cs.knowitall
package tool
package parse
package graph

import Graph._

import stem.Stemmer
import scala.util.matching.Regex

class Dependency(
    source: DependencyNode, 
    dest: DependencyNode, 
    label: String) 
extends Edge[DependencyNode](source, dest, label) {
  override def toString() = this.label + "(" + this.source + " -> " + this.dest + ")"
  def nodes = Set(source, dest)
  def otherNode(node: DependencyNode) = 
    if (source == dest) throw new IllegalStateException("source == dest")
    else if (source == node) dest
    else source
  def lemmatize(stemmer: Stemmer) = new Dependency(source.lemmatize(stemmer), dest.lemmatize(stemmer), label)
  def serialize = label + "(" + source.serialize + ", " + dest.serialize + ")"
  override def equals(other: Any) =
    other != null && other.isInstanceOf[Dependency] &&
      other.asInstanceOf[Dependency].source.equals(source) &&
      other.asInstanceOf[Dependency].dest.equals(dest) &&
      other.asInstanceOf[Dependency].label == label
  override def hashCode() = 37 * (this.source.hashCode + this.dest.hashCode * 37) + label.hashCode
}

object Dependency {
  val Serialized = new Regex("""(.+)\(\s*(.*?_.*?_.*?),\s*(.*?_.*?_.*?)\s*\)""")
  def deserialize(string: String) = {
    val Serialized(label, source, dest) = string
    new Dependency(
        DependencyNode.deserialize(source), 
        DependencyNode.deserialize(dest), 
        label)
  }
}

object Dependencies {
  def serialize(deps: Iterable[Dependency]) = deps.map(_.serialize).mkString("; ")
  def deserialize(string: String) = string.split("""\s*(?:;|\n)\s*""").map(Dependency.deserialize(_)).toList;
}

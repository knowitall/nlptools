package edu.knowitall.tool
package parse
package graph

import edu.knowitall.collection.immutable.graph.Graph._
import stem.Stemmer
import scala.util.matching.Regex
import scala.collection.immutable.SortedSet

object Dependency {
  val Serialized = new Regex("""(\p{Graph}+)\(\s*(\p{Graph}*?-\d\d*?),\s*(\p{Graph}*?-\d\d*)\s*\)""")

  object stringFormat extends Format[Dependency, String] {
    def write(dep: Dependency): String = {
      dep.label + "(" + DependencyNode.stringFormat.write(dep.source) + ", " + DependencyNode.stringFormat.write(dep.dest) + ")"
    }

    def read(pickled: String): Dependency = try {
      val Serialized(label, source, dest) = pickled
      new Dependency(
        DependencyNode.stringFormat.read(source),
        DependencyNode.stringFormat.read(dest),
        label)
    } catch {
      case e: Throwable => throw new Dependency.SerializationException("could not deserialize dependency: " + pickled, e)
    }
  }

  @deprecated("Use stringFormat instead.", "2.4.5")
  def deserialize(string: String) = stringFormat.read(string)

  class SerializationException(message: String, cause: Throwable)
  extends RuntimeException(message, cause)
}


object Dependencies {
  def serialize(deps: Iterable[Dependency]) = (deps.iterator).map(Dependency.stringFormat.write(_)).mkString("; ")
  def deserialize(string: String): Seq[Dependency] = string.split("""\s*(?:;|\n)\s*""").
      map(Dependency.stringFormat.read(_))

  object DependencyOrdering extends Ordering[Dependency] {
    def compare(a: Dependency, b: Dependency) = {
      def tuplize(dep: Dependency) =
        (dep.source.id, dep.dest.id, dep.label)
      implicitly[Ordering[(Int, Int, String)]].compare(tuplize(a), tuplize(b))
    }
  }
}

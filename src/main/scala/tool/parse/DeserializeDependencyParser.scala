package edu.washington.cs.knowitall
package tool
package parse

import graph._

/*
 * This parser "parses" a serialized dependency string into an iterable
 * of dependencies.  This is used on the nlpweb demo, for example. */
class DeserializeDependencyParser extends DependencyParser {
  override def dependencies(string: String): Iterable[Dependency] = Dependencies.deserialize(string)
}

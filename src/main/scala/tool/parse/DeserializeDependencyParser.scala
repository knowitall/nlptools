package edu.washington.cs.knowitall
package tool
package parse

import graph._

class DeserializeDependencyParser extends DependencyParser {
  override def dependencies(string: String): Iterable[Dependency] = Dependencies.deserialize(string)
}

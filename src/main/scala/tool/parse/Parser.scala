package edu.washington.cs.knowitall
package tool
package parse

import graph._

trait Parser {
  def dependencies(string: String): Iterable[Dependency]
  def dependencyGraph(string: String) = new DependencyGraph(string, dependencies(string))
}

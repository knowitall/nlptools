package edu.washington.cs.knowitall
package tool
package parse

import graph._

/** A trait for a tool that produces dependencies, such as the
  * Stanford dependency parser. */
trait DependencyParser {
  def apply(string: String) = dependencyGraph(string)

  def dependencies(string: String): Iterable[Dependency] = {
    this.dependencyGraph(string).dependencies
  }

  /**
    * Create a graph of the dependencies.  This has more information than
    * creating a DependencyGraph from an `Iterable[Dependency]` because it
    * will have the source text. */
  def dependencyGraph(string: String): DependencyGraph
}

abstract class DependencyParserMain extends LineProcessor("parser") {
  def dependencyParser: DependencyParser

  override def init(config: Config) {
    // for timing purposes
    dependencyParser.dependencies("I want to initialize the parser.")
  }

  override def process(line : String) = {
    dependencyParser.dependencyGraph(line).serialize
  }
}

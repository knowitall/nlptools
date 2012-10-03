package edu.washington.cs.knowitall
package tool
package parse

import graph._

/** A trait for a tool that produces dependencies, such as the
  * Stanford dependency parser. */
trait DependencyParser {
  def dependencies(string: String): Iterable[Dependency]

  /**
    * Create a graph of the dependencies.  This has more information than
    * creating a DependencyGraph from an `Iterable[Dependency]` because it
    * will have the source text. */
  def dependencyGraph(string: String): DependencyGraph = {
    val dependencies = this.dependencies(string)
    val nodes = dependencies.toList.flatMap(dep => List(dep.source, dep.dest))
    new DependencyGraph(string, nodes, dependencies)
  }
}

abstract class DependencyParserMain extends LineProcessor {
  def parser: DependencyParser

  override def init(args: Array[String]) {
    // for timing purposes
    parser.dependencies("I want to initialize the parser.")
  }

  override def process(line : String) = {
    parser.dependencyGraph(line).serialize
  }

  override def exit(ns: Long) {
    System.err.println(ns / 1000 / 1000 + "ms")
  }
}

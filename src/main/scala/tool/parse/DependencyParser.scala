package edu.washington.cs.knowitall
package tool
package parse

import graph._
import common.main._

trait DependencyParser {
  def dependencies(string: String): Iterable[Dependency]
  def dependencyGraph(string: String) = new DependencyGraph(string, dependencies(string))
}

abstract class DependencyParserMain extends common.main.LineProcessor {
  def parser: DependencyParser

  override def init(args: Array[String]) {
    // for timing purposes
    parser.dependencies("I want to initialize the parser.")
  }

  override def process(line : String) = {
    val deps = parser.dependencies(line)
    Dependencies.serialize(deps)
  }

  override def exit(ns: Long) {
    System.err.println(ns / 1000 / 1000 + "ms")
  }
}

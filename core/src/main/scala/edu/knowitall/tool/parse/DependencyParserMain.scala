package edu.knowitall.tool
package parse

import edu.knowitall.tool.parse.graph._
import edu.knowitall.tool.postag.Postagger

abstract class DependencyParserMain extends LineProcessor("parser") {
  def dependencyParser: DependencyParser

  override def init(config: Config) {
    // for timing purposes
    dependencyParser.dependencyGraph("I want to initialize the parser.")
  }

  override def process(line: String) = {
    val dgraph = dependencyParser.dependencyGraph(line)
    DependencyParser.multilineStringFormat.write(dgraph)
  }
}
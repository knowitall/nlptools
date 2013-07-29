package edu.knowitall
package tool
package parse

import graph._

import postag.PostaggedToken
import tokenize.Token

import scala.concurrent.ExecutionContext.Implicits.global

/** A trait for a tool that produces dependencies, such as the
  * Stanford dependency parser. */
trait DependencyParser {
  def apply(string: String) = dependencyGraph(string)

  @deprecated("Use dependencyGraph(string).dependencies", "2.4.3")
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
    dependencyParser.dependencyGraph("I want to initialize the parser.")
  }

  override def process(line : String) = {
    dependencyParser.dependencyGraph(line).serialize
  }
}

class RemoteDependencyParser(urlString: String) extends DependencyParser {
  import dispatch._
  val svc = url(urlString)

  def dependencyGraph(sentence: String) = {
    val response = Http(svc << sentence OK as.String).apply()
    DependencyGraph.deserialize(response)
  }
}

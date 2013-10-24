package edu.knowitall
package tool
package parse

import graph._

import postag.PostaggedToken
import postag.Postagger
import tokenize.Token

import scala.concurrent.ExecutionContext.Implicits.global

/** A trait for a tool that produces dependencies, such as the
  * Stanford dependency parser. */
trait DependencyParser {

  def postagger: Postagger

  /**
   * Create a graph of the dependencies from POS-tagged tokens.
   */
  def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph

  def apply(string: String) = dependencyGraph(string)

  /**
   * Create a graph of the dependencies.  This has more information than
   * creating a DependencyGraph from an `Iterable[Dependency]` because it
   * will have the source text.
   */
  def dependencyGraph(string: String): DependencyGraph = {
    val postaggedTokens = postagger.postag(string)
    dependencyGraphPostagged(postaggedTokens)
  }

  /**
   * Create a graph of the dependencies from Tokens.
   */
  def dependencyGraphTokenized(tokens: Seq[Token]) = {
    val postaggedTokens = postagger.postagTokenized(tokens)
    dependencyGraphPostagged(postaggedTokens)
  }

  @deprecated("Use dependencyGraph(string).dependencies", "2.4.3")
  def dependencies(string: String): Iterable[Dependency] = {
    this.dependencyGraph(string).dependencies
  }

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

class RemoteDependencyParser(val urlString: String) extends DependencyParser with Remote {
  override def postagger = throw new UnsupportedOperationException()

  override def dependencyGraph(sentence: String) = {
    val response = post(sentence)
    DependencyGraph.deserialize(response)
  }

  /**
   * Throws UnsupportedOperationException
   */
  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    throw new UnsupportedOperationException()
  }

  /**
   * Throws UnsupportedOperationException
   */
  override def dependencyGraphTokenized(tokens: Seq[Token]): DependencyGraph = {
    throw new UnsupportedOperationException()
  }
}

package edu.knowitall
package tool
package parse

import graph._

import postag.PostaggedToken
import postag.Postagger
import tokenize.Token

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * A trait for a tool that produces dependencies, such as the
  * Stanford dependency parser.
  */
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
  def dependencyGraph(string: String): (Seq[PostaggedToken], DependencyGraph) = {
    val postaggedTokens = postagger.postag(string)
    (postaggedTokens, dependencyGraphPostagged(postaggedTokens))
  }

  /**
    * Create a graph of the dependencies from Tokens.
    */
  def dependencyGraphTokenized(tokens: Seq[Token]): (Seq[PostaggedToken], DependencyGraph) = {
    val postaggedTokens = postagger.postagTokenized(tokens)
    (postaggedTokens, dependencyGraphPostagged(postaggedTokens))
  }
}

object DependencyParser {
  object multilineStringFormat extends Format[(Seq[PostaggedToken], DependencyGraph), String] {
    def write(dgraph: (Seq[PostaggedToken], DependencyGraph)) = {
      val (tokens, graph) = dgraph
      val tokensPickled = Postagger.multilineStringFormat.write(tokens)
      val graphPickled = DependencyGraph.multilineStringFormat.write(graph)

      tokensPickled + "\n\n" + graphPickled
    }

    def read(pickled: String): (Seq[PostaggedToken], DependencyGraph) = {
      val (postagsPickled, depsPickled) = pickled.split("\n\n") match {
        case Array(postagsPickled, depsPickled) => (postagsPickled, depsPickled)
        case _ => throw new MatchError("Could not split pickled dgraph: " + pickled)
      }

      val postags = Postagger.multilineStringFormat.read(postagsPickled)
      val graph = DependencyGraph.multilineStringFormat.read(depsPickled)

      (postags, graph)
    }
  }
}
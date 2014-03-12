package edu.knowitall.tool
package parse

import edu.knowitall.tool.chunk._
import edu.knowitall.tool.parse.graph._
import edu.knowitall.tool.postag._
import edu.knowitall.tool.tokenize._

import scala.concurrent.ExecutionContext

class RemoteDependencyParser(val urlString: String)(implicit executionContext: ExecutionContext) extends DependencyParser with Remote {
  override def postagger = throw new UnsupportedOperationException()

  override def dependencyGraph(sentence: String) = {
    val response = post(sentence)

    DependencyParser.multilineStringFormat.read(response)
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
  override def dependencyGraphTokenized(tokens: Seq[Token]): (Seq[PostaggedToken], DependencyGraph) = {
    throw new UnsupportedOperationException()
  }
}

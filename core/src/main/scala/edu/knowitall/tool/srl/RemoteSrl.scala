package edu.knowitall.tool
package srl

import edu.knowitall.tool.parse.graph._
import edu.knowitall.tool.postag.PostaggedToken
import scala.concurrent.ExecutionContext
import edu.knowitall.tool.parse.DependencyParser

class RemoteSrl(val urlString: String)(implicit executionContext: ExecutionContext) extends Srl with Remote {
  def apply(tokens: Seq[PostaggedToken], dgraph: DependencyGraph) = {
    val response = this.post(DependencyParser.multilineStringFormat.write(tokens, dgraph))
    if (response.isEmpty) Seq.empty
    else {
      response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
    }
  }
}

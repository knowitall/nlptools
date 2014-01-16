package edu.knowitall.tool
package srl

import edu.knowitall.tool.parse.graph._
import edu.knowitall.tool.postag.PostaggedToken

import scala.concurrent.ExecutionContext

class RemoteSrl(val urlString: String)(implicit executionContext: ExecutionContext) extends Srl with Remote {
  def apply(dgraph: DependencyGraph) = {
    val response = this.post(DependencyGraph.multilineStringFormat.write(dgraph))
    if (response.isEmpty) Seq.empty
    else {
      response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
    }
  }
}

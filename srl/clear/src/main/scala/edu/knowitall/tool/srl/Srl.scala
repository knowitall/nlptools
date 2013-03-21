package edu.knowitall.tool.srl

import edu.knowitall.tool.parse.graph.DependencyGraph

abstract class Srl {
  def apply(graph: DependencyGraph): Seq[Frame]
}

class RemoteSrl(urlString: String) extends Srl {
  import dispatch._
  val svc = url(urlString)

  def apply(dgraph: DependencyGraph) = {
    val response = Http(svc << dgraph.serialize OK as.String).apply()
    response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
  }
}

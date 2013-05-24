package edu.knowitall.tool.srl

import scala.concurrent.ExecutionContext.Implicits.global

import edu.knowitall.tool.LineProcessor
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.graph.DependencyGraph

abstract class Srl {
  def apply(graph: DependencyGraph): Seq[Frame]
}

abstract class SrlMain extends LineProcessor("srl") {
  def srl: Srl

  override def process(line : String) = {
    val dgraph = DependencyGraph.deserialize(line)
    (srl(dgraph) map (_.serialize)).mkString("\n")
  }
}

class RemoteSrl(urlString: String) extends Srl {
  import dispatch._
  val svc = url(urlString)

  def apply(dgraph: DependencyGraph) = {
    val response = Http(svc << dgraph.serialize OK as.String).apply()
    if (response.isEmpty) Seq.empty
    else {
      response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
    }
  }
}

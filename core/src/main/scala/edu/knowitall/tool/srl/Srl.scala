package edu.knowitall.tool
package srl

import edu.knowitall.tool.LineProcessor
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.graph.DependencyGraph
import scala.concurrent.Await
import scala.concurrent.duration._

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

class RemoteSrl(val urlString: String) extends Srl with Remote {
  def apply(dgraph: DependencyGraph) = {
    val response = this.post(urlString)
    if (response.isEmpty) Seq.empty
    else {
      response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
    }
  }
}

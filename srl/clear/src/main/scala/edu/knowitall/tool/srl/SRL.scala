package edu.knowitall.tool.srl

import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.parse.graph.DependencyGraph

abstract class Srl {
  def apply(graph: DependencyGraph): Seq[Frame]
}

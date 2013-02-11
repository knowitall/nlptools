package edu.washington.cs.knowitall.tool.srl

import edu.washington.cs.knowitall.tool.tokenize.Token
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

abstract class SRL {
  def apply(graph: DependencyGraph): Seq[Frame]
}
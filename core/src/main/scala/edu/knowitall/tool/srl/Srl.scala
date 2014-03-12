package edu.knowitall.tool
package srl

import edu.knowitall.tool.LineProcessor
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.graph.DependencyGraph
import scala.concurrent.Await
import scala.concurrent.duration._
import edu.knowitall.tool.postag.PostaggedToken

abstract class Srl {
  def apply(tokens: Seq[PostaggedToken], graph: DependencyGraph): Seq[Frame]
}

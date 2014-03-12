package edu.knowitall.tool
package srl

import edu.knowitall.tool.parse.graph._
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.parse.DependencyParser

abstract class SrlMain extends LineProcessor("srl") {
  def srl: Srl

  override def process(line: String) = {
    val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(line)
    (srl(tokens, dgraph) map (_.serialize)).mkString("\n")
  }
}

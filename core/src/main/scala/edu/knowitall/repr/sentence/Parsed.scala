package edu.knowitall.repr.sentence

import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.parse._
import edu.knowitall.tool.parse.graph._

trait Parsed {
  this: Sentence =>

  def dgraph: DependencyGraph
}

trait Parser {
  def dgraph: DependencyGraph
  def parser: DependencyParser
}

trait ParserFromString extends Parser with Postagged {
  this: Sentence =>

  def parser: DependencyParser
  override def dgraph = parser.dependencyGraph(this.text)
  override def tokens = this.dgraph.nodes.toSeq
}

trait ParserFromTokens extends Parser with Postagged {
  this: Sentence with Tokenized =>

  def parser: DependencyParser
  override def dgraph = parser.dependencyGraphTokenized(this.tokens)
  override def tokens: Seq[PostaggedToken] = this.dgraph.nodes.toSeq
}

trait ParserFromPostagged extends Parser {
  this: Sentence with Postagged =>

  def parser: DependencyParser
  override def dgraph = parser.dependencyGraphPostagged(this.tokens)
}

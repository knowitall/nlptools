package edu.knowitall.repr.sentence

import edu.knowitall.tool.tokenize._
import edu.knowitall.tool.postag._
import edu.knowitall.tool.chunk._
import edu.knowitall.tool.stem._

trait TokensSupertrait {
  this: Sentence =>
  type token <: Token

  def tokens: Seq[token]

  def strings: Seq[String] = tokens.map(_.string)
}

trait Tokens extends TokensSupertrait {
  this: Sentence =>
  type token = Token
}

trait Tokenizer extends Tokens {
  this: Sentence =>

  def tokenizer: edu.knowitall.tool.tokenize.Tokenizer

  override lazy val tokens: Seq[Token] =
    tokenizer.tokenize(text)
}

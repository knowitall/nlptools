package edu.knowitall.repr.sentence

import edu.knowitall.tool.tokenize._
import edu.knowitall.tool.postag._
import edu.knowitall.tool.chunk._
import edu.knowitall.tool.stem._

trait TokenizedSupertrait {
  this: Sentence =>
  type token <: Token

  def tokens: Seq[token]
  def tokenizedTokens: Seq[Token]
}

trait Tokenized extends TokenizedSupertrait {
  this: Sentence =>
  type token = Token

  override def tokens: Seq[token] = tokenizedTokens
}

trait Tokenizer extends Tokenized {
  this: Sentence =>

  def tokenizer: edu.knowitall.tool.tokenize.Tokenizer
  override lazy val tokenizedTokens: Seq[Token] = tokenizer.tokenize(text)
}

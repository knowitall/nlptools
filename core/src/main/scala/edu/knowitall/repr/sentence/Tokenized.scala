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

  def strings: Seq[String] = tokens.map(_.string)
}

trait Tokenized extends TokenizedSupertrait {
  this: Sentence =>
  type token = Token

  override def tokens: Seq[token] = tokenizedTokens
}

trait Tokenizer extends Tokenized {
  this: Sentence =>

  def tokenizer: edu.knowitall.tool.tokenize.Tokenizer

  def postTokenize(tokens: Seq[Token]): Seq[Token] = tokens
  override lazy val tokenizedTokens: Seq[Token] = 
    postTokenize(tokenizer.tokenize(text))
}

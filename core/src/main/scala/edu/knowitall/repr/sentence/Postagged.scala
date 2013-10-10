package edu.knowitall.repr.sentence

import edu.knowitall.tool.postag._

trait PostaggedSupertrait extends TokenizedSupertrait {
  this: Sentence =>

  def postaggedTokens: Seq[PostaggedToken]
  type token <: PostaggedToken

  override def tokenizedTokens = postaggedTokens
}

trait Postagged extends PostaggedSupertrait {
  this: Sentence =>

  type token = PostaggedToken
  override def tokens: Seq[token] = postaggedTokens
}

trait Postagger extends Postagged {
  this: Sentence =>
  def postagger: edu.knowitall.tool.postag.Postagger
  override lazy val postaggedTokens: Seq[PostaggedToken] = postagger.postag(this.text)
}


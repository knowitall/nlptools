package edu.knowitall.repr.sentence

import edu.knowitall.tool.postag._

trait PostaggedSupertrait extends TokenizedSupertrait {
  this: Sentence =>

  def postaggedTokens: Seq[PostaggedToken]
  type token <: PostaggedToken

  override def tokenizedTokens = postaggedTokens

  def postags: Seq[String] = postaggedTokens.map(_.postag)
}

trait Postagged extends PostaggedSupertrait {
  this: Sentence =>

  type token = PostaggedToken
  override def tokens: Seq[token] = postaggedTokens
}

trait Postagger extends Postagged {
  this: Sentence =>
  def postagger: edu.knowitall.tool.postag.Postagger

  def postPostag(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = tokens
  override lazy val postaggedTokens: Seq[PostaggedToken] = 
    postPostag(postagger.postag(this.text))
}


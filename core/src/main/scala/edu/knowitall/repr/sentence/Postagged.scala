package edu.knowitall.repr.sentence

import edu.knowitall.tool.postag._

trait PostaggedSupertrait extends TokenizedSupertrait {
  this: Sentence =>

  type token <: PostaggedToken

  def postags: Seq[String] = tokens.map(_.postag)
}

trait Postagged extends PostaggedSupertrait {
  this: Sentence =>

  type token = PostaggedToken
}

trait Postagger extends Postagged {
  this: Sentence =>
  def postagger: edu.knowitall.tool.postag.Postagger

  def postPostag(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = tokens
  override lazy val tokens: Seq[PostaggedToken] =
    postPostag(postagger.postag(this.text))
}


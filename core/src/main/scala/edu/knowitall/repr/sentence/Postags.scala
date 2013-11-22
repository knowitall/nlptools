package edu.knowitall.repr.sentence

import edu.knowitall.tool.postag._

trait PostagsSupertrait extends TokensSupertrait {
  this: Sentence =>

  type token <: PostaggedToken

  def postags: Seq[String] = tokens.map(_.postag)
}

trait Postags extends PostagsSupertrait {
  this: Sentence =>

  type token = PostaggedToken
}

trait Postagger extends Postags {
  this: Sentence =>
  def postagger: edu.knowitall.tool.postag.Postagger

  override lazy val tokens: Seq[PostaggedToken] =
    postagger.postag(this.text)
}


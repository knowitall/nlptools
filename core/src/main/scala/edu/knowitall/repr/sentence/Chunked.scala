package edu.knowitall.repr.sentence

import edu.knowitall.tool.chunk._

trait ChunkedSupertrait extends PostaggedSupertrait {
  this: Sentence =>

  type token <: ChunkedToken
  def chunkedTokens: Seq[ChunkedToken]
  
  override def postaggedTokens = chunkedTokens
}

trait Chunked extends ChunkedSupertrait {
  this: Sentence =>

  type token = ChunkedToken
  override lazy val tokens: Seq[token] = chunkedTokens
}

trait Chunker extends Chunked {
  this: Sentence =>

  def chunker: edu.knowitall.tool.chunk.Chunker
  override def chunkedTokens: Seq[ChunkedToken] = chunker.chunk(this.text)
}

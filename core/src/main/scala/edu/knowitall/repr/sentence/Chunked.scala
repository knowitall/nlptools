package edu.knowitall.repr.sentence

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.chunk._

trait ChunkedSupertrait extends PostaggedSupertrait {
  this: Sentence =>

  type token <: ChunkedToken
  def chunkedTokens: Seq[ChunkedToken]
  
  override def postaggedTokens = chunkedTokens

  def chunks: Seq[String] = postaggedTokens.map(_.chunk)
  def chunkIntervals: Seq[(String, Interval)] = Chunker.intervals(chunkedTokens)
}

trait Chunked extends ChunkedSupertrait {
  this: Sentence =>

  type token = ChunkedToken
  override lazy val tokens: Seq[token] = chunkedTokens
}

trait Chunker extends Chunked {
  this: Sentence =>

  def postChunk(tokens: Seq[ChunkedToken]): Seq[ChunkedToken] = {
    Chunker.joinPos(Chunker.joinOf(tokens))
  }

  def chunker: edu.knowitall.tool.chunk.Chunker
  override def chunkedTokens: Seq[ChunkedToken] =
    postChunk(chunker.chunk(this.text))
}

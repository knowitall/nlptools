package edu.knowitall.repr.sentence

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.chunk._

trait ChunksSupertrait extends PostagsSupertrait {
  this: Sentence =>

  type token <: ChunkedToken

  def chunks: Seq[String] = tokens.map(_.chunk)
  def chunkIntervals: Seq[(String, Interval)] = Chunker.intervals(tokens)
}

trait Chunks extends ChunksSupertrait {
  this: Sentence =>

  type token = ChunkedToken
}

trait Chunker extends Chunks {
  this: Sentence =>

  def chunker: edu.knowitall.tool.chunk.Chunker
  override lazy val tokens: Seq[ChunkedToken] =
    chunker.chunk(this.text)
}

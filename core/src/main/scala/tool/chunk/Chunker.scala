package edu.washington.cs.knowitall
package tool
package chunk

import common.main.LineProcessor

/** A Chunker takes postagged text and adds a chunk tag, specifying
  * whether a noun or verb phrase is starting or continuing.
  */
abstract class Chunker(val postagger: postag.PosTagger) {
  /* chunk postagged text */
  def chunkPostagged(tokens: Seq[postag.PostaggedToken]): Seq[ChunkedToken]

  /* chunk tokenized text */
  def chunkTokenized(tokens: Seq[tokenize.Token]): Seq[ChunkedToken] = {
    val postags = postagger.postagTokens(tokens)
    chunkPostagged(postags)
  }

  /* chunk raw text */
  def chunk(sentence: String): Seq[ChunkedToken] = {
    val postags = postagger.postag(sentence)
    chunkPostagged(postags)
  }
}

abstract class ChunkerMain
extends LineProcessor {
  def chunker: Chunker
  override def process(line: String) = chunker.chunk(line).map { case ChunkedToken(chunk, postag, string, offset) =>
    string + "/" + postag + "/" + chunk
  }.mkString(" ")

  override def init(args: Array[String]) {
    // for timing purposes
    chunker.chunk("I want to initialize the chunker.")
  }

  override def exit(ns: Long) {
    System.err.println(ns / 1000 / 1000 + "ms")
  }
}

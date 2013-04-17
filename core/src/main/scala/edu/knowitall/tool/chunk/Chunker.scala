package edu.knowitall
package tool
package chunk

import edu.knowitall.collection.immutable.Interval

/** A Chunker takes postagged text and adds a chunk tag, specifying
  * whether a noun or verb phrase is starting or continuing.
  */
abstract class Chunker(val postagger: postag.Postagger) {
  def apply(sentence: String) = chunk(sentence)

  /** chunk postagged text */
  def chunkPostagged(tokens: Seq[postag.PostaggedToken]): Seq[ChunkedToken]

  /** chunk tokenized text */
  def chunkTokenized(tokens: Seq[tokenize.Token]): Seq[ChunkedToken] = {
    val postags = postagger.postagTokens(tokens)
    chunkPostagged(postags)
  }

  /** chunk raw text */
  def chunk(sentence: String): Seq[ChunkedToken] = {
    val postags = postagger.postag(sentence)
    chunkPostagged(postags)
  }
}

object Chunker {
  def joinOf(chunks: Seq[ChunkedToken]): Seq[ChunkedToken] = {
    var mutableChunks = chunks

    for (index <- Range(0, chunks.size)) {
      val chunk = chunks(index)
      if (chunk.string.toLowerCase == "of" && chunk.postag == "IN" &&
          (index > 0 && (chunks(index - 1).chunk endsWith "NP")) &&
          (index < chunks.length && chunks(index + 1).chunk == "B-NP")) {
        val nextChunk = chunks(index + 1)
        mutableChunks = mutableChunks.updated(index, new ChunkedToken("I-NP", chunk.postag, chunk.string, chunk.offset))
        mutableChunks = mutableChunks.updated(index + 1, new ChunkedToken("I-NP", nextChunk.postag, nextChunk.string, nextChunk.offset))
      }
    }

    mutableChunks
  }

  def intervals(chunks: Seq[ChunkedToken]): Seq[(String, Interval)] = {
    def helper(chunks: Iterator[String]) = {
      var intervals = Vector.empty[(String, Interval)]
      var iterator = chunks.zipWithIndex
      while (iterator.hasNext) {
        val interval = {
          val (nextToken, nextIndex) = iterator.next()

          val (chunkTokens, rest) = iterator.span(token => token._1 startsWith "I")
          iterator = rest

          val chunkType = {
            val hyphen = nextToken.indexOf('-')
            if (hyphen == -1) nextToken
            else nextToken.drop(hyphen + 1)
          }
          (chunkType, Interval.open(nextIndex, chunkTokens.toSeq.lastOption.map(_._2 + 1).getOrElse(nextIndex + 1)))
        }

        intervals = intervals :+ interval
      }

      intervals
    }

    helper(chunks.iterator.map(_.chunk))
  }
}

abstract class ChunkerMain
extends LineProcessor("chunker") {
  def chunker: Chunker
  override def process(line: String) = chunker.chunk(line).map { case ChunkedToken(chunk, postag, string, offset) =>
    string + "/" + postag + "/" + chunk
  }.mkString(" ")

  override def init(config: Config) {
    // for timing purposes
    chunker.chunk("I want to initialize the chunker.")
  }
}

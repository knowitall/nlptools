package edu.washington.cs.knowitall
package tool
package chunk

import opennlp.tools.chunker._

class OpenNlpChunker(
  val model: ChunkerModel,
  postagger: postag.Postagger)
extends Chunker(postagger) {
  def this(modelName: String = "en-chunker.bin",
    postagger: postag.Postagger = new postag.OpenNlpPostagger()) =
    this(new ChunkerModel(OpenNlpChunker.loadModel(modelName)), postagger)

  val chunker = new ChunkerME(model)

  def chunkPostagged(tokens: Seq[postag.PostaggedToken]): Seq[ChunkedToken] = {
    val chunks = chunker.chunk(tokens.map(_.string).toArray, tokens.map(_.postag).toArray)
    (tokens zip chunks) map { case (token, chunk) => new ChunkedToken(token, chunk) }
  }
}

object OpenNlpChunker {
  private def loadModel(name: String) = {
    val resource = classOf[OpenNlpChunker].getClassLoader.getResourceAsStream(name)
    if (resource == null) throw new IllegalArgumentException("could not find resource: " + name)
    else resource
  }
}

object OpenNlpChunkerMain extends ChunkerMain {
  lazy val chunker = new OpenNlpChunker("en-chunker.bin", new postag.OpenNlpPostagger())
}

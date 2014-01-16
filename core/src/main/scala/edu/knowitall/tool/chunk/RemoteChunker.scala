package edu.knowitall.tool
package chunk

import edu.knowitall.tool.postag._
import edu.knowitall.tool.tokenize._

import scala.concurrent.ExecutionContext

class RemoteChunker(val urlString: String)(implicit executionContext: ExecutionContext) extends Chunker with Remote {
  override def postagger = throw new UnsupportedOperationException()
  override def chunkPostagged(tokens: Seq[PostaggedToken]) = throw new UnsupportedOperationException()
  override def chunkTokenized(tokens: Seq[Token]) = throw new UnsupportedOperationException()
  override def chunk(sentence: String) = {
    val response = post(sentence)
    Chunker.multilineStringFormat.read(response)
  }
}

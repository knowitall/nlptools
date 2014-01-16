package edu.knowitall.tool
package postag

import edu.knowitall.tool.tokenize._

import scala.concurrent.ExecutionContext

class RemotePostagger(val urlString: String)(implicit executionContext: ExecutionContext) extends Postagger with Remote {
  override def tokenizer = throw new UnsupportedOperationException()
  override def postagTokenized(tokens: Seq[Token]) = throw new UnsupportedOperationException()
  override def postag(sentence: String) = {
    val response = post(sentence)
    Postagger.multilineStringFormat.read(response)
  }
}

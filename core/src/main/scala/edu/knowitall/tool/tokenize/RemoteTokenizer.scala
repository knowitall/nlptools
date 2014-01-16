package edu.knowitall.tool
package tokenize

import scala.concurrent.ExecutionContext

class RemoteTokenizer(val urlString: String)(implicit executionContext: ExecutionContext) extends Tokenizer with Remote {
  def tokenize(sentence: String) = {
    val response = post(sentence)
    Tokenizer.multilineStringFormat.read(response)
  }
}

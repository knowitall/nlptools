package edu.knowitall.tool
package stem

import scala.concurrent.ExecutionContext

class RemoteStemmer(val urlString: String)(implicit executionContext: ExecutionContext) extends Stemmer with Remote {
  override def stem(word: String) = {
    post(word)
  }
}

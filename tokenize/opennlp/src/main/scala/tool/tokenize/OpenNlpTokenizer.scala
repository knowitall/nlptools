package edu.washington.cs.knowitall
package tool
package tokenize

import common.main.LineProcessor
import scala.collection.JavaConversions._
import opennlp.tools.tokenize.{ TokenizerME, TokenizerModel }
import opennlp.tools.util.Span

class OpenNlpTokenizer(val model: TokenizerModel) extends Tokenizer {
  def this(modelName: String = "en-token.bin") =
    this(new TokenizerModel(OpenNlpTokenizer.loadModel(modelName)))

  val tokenizer = new TokenizerME(model)

  def tokenize(sentence: String): Seq[Token] = {
    val positions = tokenizer.tokenizePos(sentence)
    val strings = positions.map {
      pos => sentence.substring(pos.getStart, pos.getEnd)
    }

    assume(positions.length == strings.length)

    for ((pos, string) <- (positions.iterator zip strings.iterator).toSeq)
    yield new Token(string, pos.getStart)
  }
}

object OpenNlpTokenizer extends LineProcessor {
  val tokenizer = new OpenNlpTokenizer()
  override def process(sentence: String) =
    tokenizer.tokenize(sentence).mkString(" ")

  private def loadModel(name: String) = {
    val resource = classOf[OpenNlpTokenizer].getClassLoader.getResourceAsStream(name)
    if (resource == null) throw new IllegalArgumentException("could not find resource: " + name)
    else resource
  }
}

package edu.washington.cs.knowitall
package tool
package tokenize

/** A tokenizer takes a sentence string as input and
  * seperates words (tokens) along word (token) boundaries.
  */
trait Tokenizer {
  def apply(sentence: String) = tokenize(sentence)
  def tokenize(sentence: String): Seq[Token]
}

object Tokenizer {
  /** This method takes tokenized strings and the source sentence.
    * It adds offset information to the strings by tracing through
    * the source sentence and skipping whitespace. */
  def computeOffsets(strings: Iterable[String], sentence: String) = {
    var sent: Array[Char] = sentence.toCharArray()
    var offset: Int = 0
    var tokens: Seq[Token] = Seq.empty

    for (string <- strings) {
      val leftOffset = offset
      assume(sent startsWith string)

      sent = sent.drop(string.length)
      val skip = sent.takeWhile(_.isWhitespace).length
      sent = sent.drop(skip)

      offset += string.length + skip
      tokens = tokens :+ new Token(string, leftOffset)
    }

    tokens
  }
}

abstract class TokenizerMain extends LineProcessor("tokenizer") {
  def tokenizer: Tokenizer
  override def process(sentence: String) =
    tokenizer.tokenize(sentence).mkString(" ")
}

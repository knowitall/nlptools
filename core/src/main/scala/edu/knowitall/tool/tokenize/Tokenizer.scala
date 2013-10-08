package edu.knowitall
package tool
package tokenize

import scala.concurrent.ExecutionContext.Implicits.global

/** A tokenizer takes a sentence string as input and
  * seperates words (tokens) along word (token) boundaries.
  */
trait Tokenizer {
  def apply(sentence: String): Seq[Token] = tokenize(sentence)
  def tokenize(sentence: String): Seq[Token]
}

object Tokenizer {
  /** This method takes tokenized strings and the source sentence.
    * It adds offset information to the strings by tracing through
    * the source sentence and skipping whitespace. */
  def computeOffsets(strings: TraversableOnce[String], sentence: String) = {
    var sent: Array[Char] = sentence.toCharArray()
    var offset: Int = 0
    var tokens: Seq[Token] = Seq.empty

    // remove leading spaces
    val (spaces, rest) = sent.span(c => c.isWhitespace || c.isControl)
    offset += spaces.size
    sent = rest

    for (string <- strings) {
      val leftOffset = offset
      assume(sent startsWith string, "Wrong sentence prefix: '" + string + "' of " + "'" + sentence + "'")

      sent = sent.drop(string.length)
      val skip = sent.takeWhile(c => c.isWhitespace || c.isControl).length
      sent = sent.drop(skip)

      offset += string.length + skip
      tokens = tokens :+ Token(string, leftOffset)
    }

    tokens
  }

  /** Rebuild the original text from tokens.  This will maintain
    * the original spacing, although different forms of spacing
    * such as tabs will become standard spaces. Optional argument
    * startOffset specifies offset at which the original text
    * began, which is useful for controlling whitespace
    * at beginning of the rebuilt original text string. */
  def originalText(tokens: TraversableOnce[Token], startOffset: Int = 0) = {
    val builder = new StringBuilder()

    for (token <- tokens) {
      require(token.offset >= startOffset,
          "Token must have offset >= startOffset. " +
          "Given offset=" + token.offset + ", startOffset=" + startOffset)
      builder.append(" " * (token.offset - builder.length - startOffset))
      builder.append(token.string)
    }

    builder.toString()
  }

  private[this] val tokenRegex = """(.+)@(\d+)""".r
  def deserialize(pickled: String): Seq[Token] = {
    val split = pickled.split("\\s+")
    split.map {
      case tokenRegex(string, offset) => Token(string, offset.toInt)
      case s => throw new MatchError("Could not deserialize: " + s)
    }(scala.collection.breakOut)
  }
}

abstract class TokenizerMain extends LineProcessor("tokenizer") {
  def tokenizer: Tokenizer
  override def process(sentence: String) =
    tokenizer.tokenize(sentence).mkString(" ")
}

class RemoteTokenizer(urlString: String) extends Tokenizer {
  import dispatch._
  val svc = url(urlString)

  def tokenize(sentence: String) = {
    val response = Http(svc << sentence OK as.String).apply()
    Tokenizer.deserialize(response)
  }
}

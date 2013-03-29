package edu.knowitall
package tool
package tokenize

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import edu.knowitall.collection.immutable.graph.Graph._
import edu.knowitall.tool.stem.Stemmer

@RunWith(classOf[JUnitRunner])
object TokenizerSpecTest extends Specification {
  "offsets are computed correctly and the original text is inferred" in {
    val sentence = "John walks down the hall."
    val tokens = Tokenizer.computeOffsets(Seq("John", "walks", "down", "the", "hall", "."), sentence)

    // make sure offsets were computed correctly
    tokens.map(_.offsets.start) must_== Seq(0, 5, 11, 16, 20, 24)

    // make sure we can go back to the original sentence
    Tokenizer.originalText(tokens) must_== sentence
  }

  "offsets are computed correctly and the original text is inferred when there is a leading space" in {
    val sentence = "  John walks down the hall."
    val tokens = Tokenizer.computeOffsets(Seq("John", "walks", "down", "the", "hall", "."), sentence)

    // make sure offsets were computed correctly
    tokens.map(_.offsets.start) must_== Seq(2, 7, 13, 18, 22, 26)

    // make sure we can go back to the original sentence
    Tokenizer.originalText(tokens) must_== sentence
  }
}

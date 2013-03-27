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
  "offsets are computed correctly" in {
    val tokens = Tokenizer.computeOffsets(Seq("John", "walks", "down", "the", "hall", "."), "John walks down the hall.")
    tokens.map(_.offsets.start) must_== Seq(0, 5, 11, 16, 20, 24)
  }

  "offsets are computed correctly when there is a leading space" in {
    val tokens = Tokenizer.computeOffsets(Seq("John", "walks", "down", "the", "hall", "."), "  John walks down the hall.")
    tokens.map(_.offsets.start) must_== Seq(2, 7, 13, 18, 22, 26)
  }
}

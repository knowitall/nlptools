package edu.knowitall
package tool
package tokenize

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object ClearParserTest extends Specification {
  "tokenize example sentence" in {
    val text = "This is a test of Clear's tokenizer."
    val tokenizer = new ClearTokenizer()
    tokenizer(text).mkString(" ") must_== "This@0 is@5 a@8 test@10 of@15 Clear@18 's@23 tokenizer@26 .@35"
  }
}


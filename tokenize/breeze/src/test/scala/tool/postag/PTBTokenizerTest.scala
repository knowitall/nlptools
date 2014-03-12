package edu.knowitall
package tool
package tokenize

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object PTBTokenizerTest extends Specification {
  "tokenize example sentence" in {
    val text = "This   is a test of the  breeze PTBTokenizer."
    val tokenizer = new PTBTokenizer()

    val tokenized = tokenizer.tokenize(text)
    tokenized.mkString("; ") must_== "This 0; is 7; a 10; test 12; of 17; the 20; breeze 25; PTBTokenizer 32; . 44"

    tokenizer.tokenize("Mr. Obama's campaign.").mkString("; ") must_== "Mr. 0; Obama 4; 's 9; campaign 12; . 20"
  }
}


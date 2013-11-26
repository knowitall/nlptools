package edu.knowitall
package tool
package chunk

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object OpenNlpChunkerTest extends Specification {
  "chunk example sentence" in {
    val text = "This is a test of the OpenNlp chunker."
    val chunker = new OpenNlpChunker

    val chunked = chunker.chunk(text)
    chunked.mkString("; ") must_== "This 0 DT B-NP; is 5 VBZ B-VP; a 8 DT B-NP; test 10 NN I-NP; of 15 IN B-PP; the 18 DT B-NP; OpenNlp 22 NNP I-NP; chunker 30 NN I-NP; . 37 . O"
  }
}


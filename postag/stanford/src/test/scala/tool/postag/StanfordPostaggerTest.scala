package edu.washington.cs.knowitall
package tool
package postag

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object StanfordParserTest extends Specification {
  "postag example sentence" in {
    val text = "This is a test of the Stanford postagger."
    val postagger = new StanfordPostagger

    val postagged = postagger.postag(text)
    postagged.mkString(" ") must_== "This/DT@0 is/VBZ@5 a/DT@8 test/NN@10 of/IN@15 the/DT@18 Stanford/NNP@22 postagger/NN@31 ./.@40"
  }
}


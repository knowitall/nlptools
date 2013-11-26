package edu.knowitall
package tool
package postag

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object StanfordPostaggerTest extends Specification {
  "postag example sentence" in {
    val text = "This is a test of the Stanford postagger."
    val postagger = new StanfordPostagger

    val postagged = postagger.postag(text)
    postagged.mkString("; ") must_== "This 0 DT; is 5 VBZ; a 8 DT; test 10 NN; of 15 IN; the 18 DT; Stanford 22 NNP; postagger 31 NN; . 40 ."
  }
}


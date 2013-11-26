package edu.knowitall
package tool
package postag

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object OpenNlpPostaggerTest extends Specification {
  "postag example sentence" in {
    val text = "This is a test of the OpenNlp postagger."
    val postagger = new OpenNlpPostagger

    val postagged = postagger.postag(text)
    postagged.mkString("; ") must_== "This 0 DT; is 5 VBZ; a 8 DT; test 10 NN; of 15 IN; the 18 DT; OpenNlp 22 NNP; postagger 30 NN; . 39 ."
  }
}


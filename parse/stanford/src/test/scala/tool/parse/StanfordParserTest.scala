package edu.knowitall
package tool
package parse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object StanfordParserTest extends Specification {
  "constituency parse example sentence" in {
    val text = "This is a test of the Stanford Parser."
    val parser = new StanfordParser

    val constituency = parser.parse(text)
    constituency.toString must_== "(ROOT (S (NP (DT This)) (VP (VBZ is) (NP (NP (DT a) (NN test)) (PP (IN of) (NP (DT the) (NNP Stanford) (NNP Parser))))) (. .)))"
  }

  "dependency parse example sentence" in {
    val text = "This is a test of the Stanford Parser."
    val parser = new StanfordParser

    val dependency = parser.dependencyGraph(text, BaseStanfordParser.CCCompressed)
    dependency.toString must_== "nsubj(test-3, This-0); cop(test-3, is-1); det(test-3, a-2); det(Parser-7, the-5); nn(Parser-7, Stanford-6); prep_of(test-3, Parser-7)"
  }
}


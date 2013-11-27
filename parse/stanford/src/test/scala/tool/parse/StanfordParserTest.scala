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
    dependency.toString must_== "nsubj(test-4, This-1); cop(test-4, is-2); det(test-4, a-3); root(ROOT-0, test-4); det(Parser-8, the-6); nn(Parser-8, Stanford-7); prep_of(test-4, Parser-8)"
  }
}


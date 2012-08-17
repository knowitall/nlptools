package edu.washington.cs.knowitall
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
    dependency.toString must_== "(of_IN_4_15); (._._8_37); nsubj(test_NN_3_10, This_DT_0_0); cop(test_NN_3_10, is_VBZ_1_5); det(test_NN_3_10, a_DT_2_8); prep_of(test_NN_3_10, Parser_NNP_7_31); det(Parser_NNP_7_31, the_DT_5_18); nn(Parser_NNP_7_31, Stanford_NNP_6_22)"
  }
}


package edu.washington.cs.knowitall
package tool
package parse

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object BllipParserSpecTest extends Specification {
  "constituency parse example sentence" in {
    val text = "This is a test of the Stanford Parser."
    val parser = new BllipParser

    val constituency = parser.parse(text)
    constituency.toString must_== "(S1 (S (NP (DT This)) (VP (AUX is) (NP (NP (DT a) (NN test)) (PP (IN of) (NP (DT the) (NNP Stanford) (NNP Parser))))) (. .)))"
  }

  "dependency parse example sentence" in {
    val text = "This is a test of the Stanford Parser."
    val parser = new BllipParser

    val dependency = parser.dependencyGraph(text, BaseStanfordParser.None)
    dependency.toString must_== "(._._8_37); nsubj(test_NN_3_10, This_DT_0_0); cop(test_NN_3_10, is_AUX_1_5); det(test_NN_3_10, a_DT_2_8); prep(test_NN_3_10, of_IN_4_15); pobj(of_IN_4_15, Parser_NNP_7_31); det(Parser_NNP_7_31, the_DT_5_18); nn(Parser_NNP_7_31, Stanford_NNP_6_22)"
  }
}


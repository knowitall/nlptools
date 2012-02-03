package edu.washington.cs.knowitall
package tool
package parse
package pattern

import graph._

import org.junit._
import org.junit.Assert._
import org.specs.Specification
import org.specs.runner.JUnit4
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner

import Graph._

@RunWith(classOf[JUnitSuiteRunner])
class DependencyPatternSpecTest extends JUnit4(DependencyPatternSpec)
object DependencyPatternSpec extends Specification {
  def testPostagConstraint {
    val sentence = "Angels appear in the Bible story from the first pages of Genesis right through to the final pages of the Book of Revelation ."
    val dgraph = DependencyGraph.deserialize("nsubj(appear_VB_1, Angels_NNPS_0); det(story_NN_5, the_DT_3); nn(story_NN_5, Bible_NNP_4); prep_in(appear_VB_1, story_NN_5); det(pages_NNS_9, the_DT_7); amod(pages_NNS_9, first_JJ_8); prep_from(appear_VB_1, pages_NNS_9); nn(right_NN_12, Genesis_NNP_11); prep_of(pages_NNS_9, right_NN_12); dep(appear_VB_1, through_IN_13); dep(through_IN_13, to_TO_14); det(pages_NNS_17, the_DT_15); amod(pages_NNS_17, final_JJ_16); pobj(to_TO_14, pages_NNS_17); det(Book_NNP_20, the_DT_19); prep_of(pages_NNS_17, Book_NNP_20); prep_of(Book_NNP_20, Revelation_NNP_22); punct(appear_VB_1, ._._23)").normalize
     
    "(Angels, appear, the Bible) is found without a postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >prep_in> {arg2}")
      val matches = pattern(dgraph.graph)
      matches.size must be_>(0)
    }
    
    "(Angels, appear, the Bible) is found with a postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:VB} >prep_in> {arg2}")
      val matches = pattern(dgraph.graph)
      matches.size must_== 1
    }
    
    "(Angels, appear, the Bible) is not found with the wrong postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:XXX} >prep_in> {arg2}")
      val matches = pattern(dgraph.graph)
      matches.size must_== 0
    }
  }

  testPostagConstraint
}

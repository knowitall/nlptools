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

import collection.immutable.graph._
import collection.immutable.graph.pattern._
import collection.immutable.graph.Graph._

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
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:postag=VB} >prep_in> {arg2}")
      val matches = pattern(dgraph.graph)
      matches.size must_== 1
    }
    
    "(Angels, appear, the Bible) is not found with the wrong postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:postag=XXX} >prep_in> {arg2}")
      val matches = pattern(dgraph.graph)
      matches.size must_== 0
    }
  }

  testPostagConstraint

  def testDeserialize(pickled: String, pattern: DependencyPattern) = {
    pickled + " deserializes properly" in {
      val unpickled = DependencyPattern.deserialize(pickled)
      unpickled.matchers must_== pattern.matchers
      unpickled.toString must_== pickled
      unpickled must_== pattern
    }

  }
  
  testDeserialize("{arg1} <nn< {rel} >nn> {arg2}", new DependencyPattern(List(
        new CaptureNodeMatcher[DependencyNode]("arg1"),
        new DirectedEdgeMatcher[DependencyNode](Direction.Up, new LabelEdgeMatcher("nn")),
        new CaptureNodeMatcher[DependencyNode]("rel"),
        new DirectedEdgeMatcher[DependencyNode](Direction.Down, new LabelEdgeMatcher("nn")),
        new CaptureNodeMatcher[DependencyNode]("arg2"))))
        
  testDeserialize("{capture}", new DependencyPattern(List(
        new CaptureNodeMatcher[DependencyNode]("capture"))))
        
  testDeserialize("{capture:text=bar}", new DependencyPattern(List(
        new CaptureNodeMatcher[DependencyNode]("capture", new TextNodeMatcher("bar")))))
        
  testDeserialize("{capture:postag=foo}", new DependencyPattern(List(
        new CaptureNodeMatcher[DependencyNode]("capture", new PostagNodeMatcher("foo")))))
        
  testDeserialize("{capture:regex=asdf}", new DependencyPattern(List(
        new CaptureNodeMatcher[DependencyNode]("capture", new RegexNodeMatcher("asdf".r)))))
        
  testDeserialize("{capture:regex=asdf:postag=foo}", new DependencyPattern(List(
        new CaptureNodeMatcher[DependencyNode]("capture", new ConjunctiveNodeMatcher(new RegexNodeMatcher("asdf".r), new PostagNodeMatcher("foo"))))))
        
  testDeserialize("regex=asdf", new DependencyPattern(List(
        new RegexNodeMatcher("asdf".r))))
}

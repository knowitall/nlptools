package edu.knowitall
package tool
package parse
package pattern

import graph._

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import edu.knowitall.collection.immutable.graph._
import edu.knowitall.collection.immutable.graph.pattern._
import edu.knowitall.collection.immutable.graph.Graph._

import tool.stem.IdentityStemmer.instance

@RunWith(classOf[JUnitRunner])
object DependencyPatternSpecTest extends Specification {
  def testPostagConstraint {
    val sentence = "Angels appear in the Bible story from the first pages of Genesis right through to the final pages of the Book of Revelation ."
    val dgraph = DependencyGraph.deserialize("nsubj(appear_VB_1_0, Angels_NNPS_0_0); det(story_NN_5_0, the_DT_3_0); nn(story_NN_5_0, Bible_NNP_4_0); prep_in(appear_VB_1_0, story_NN_5_0); det(pages_NNS_9_0, the_DT_7_0); amod(pages_NNS_9_0, first_JJ_8_0); prep_from(appear_VB_1_0, pages_NNS_9_0); nn(right_NN_12_0, Genesis_NNP_11_0); prep_of(pages_NNS_9_0, right_NN_12_0); dep(appear_VB_1_0, through_IN_13_0); dep(through_IN_13_0, to_TO_14_0); det(pages_NNS_17_0, the_DT_15_0); amod(pages_NNS_17_0, final_JJ_16_0); pobj(to_TO_14_0, pages_NNS_17_0); det(Book_NNP_20_0, the_DT_19_0); prep_of(pages_NNS_17_0, Book_NNP_20_0); prep_of(Book_NNP_20_0, Revelation_NNP_22_0); punct(appear_VB_1_0, ._._23_0)").normalize

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

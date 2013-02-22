package edu.washington.cs.knowitall.tool.srl

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

@RunWith(classOf[JUnitRunner])
object FrameHierarchySpecTest extends Specification {
  /*
  {
    val sentence = "John ran down the hill."
    "no nested frames are found in: '" + sentence + "'" in {
      val dgraph = DependencyGraph.deserialize("nsubj(ran_VBD_1_5, John_NNP_0_0); prep(ran_VBD_1_5, down_IN_2_9); punct(ran_VBD_1_5, ._._5_22); pobj(down_IN_2_9, hill_NN_4_18); det(hill_NN_4_18, the_DT_3_14)")
      val frame = Frame.deserialize(dgraph)("run_1.02:[A0=John_0, AM_DIR=down_2]")
      val hierarchy = FrameHierarchy.fromFrames(dgraph, IndexedSeq(frame))
      hierarchy.size must_== 1
      hierarchy.head.children.size must_== 0
    }
  }
  */

  {
    val sentence = "John wants to run down the hill."
    ("one nested frame is found in: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(wants_VBZ_1_5, John_NNP_0_0); xcomp(wants_VBZ_1_5, run_VB_3_14); punct(wants_VBZ_1_5, ._._7_31); aux(run_VB_3_14, to_TO_2_11); prep(run_VB_3_14, down_IN_4_18); pobj(down_IN_4_18, hill_NN_6_27); det(hill_NN_6_27, the_DT_5_23)")
      val frames = IndexedSeq("run_3.02:[A0=John_0, AM_DIR=down_4]", "want_1.01:[A0=John_0, A1=run_3]") map Frame.deserialize(dgraph)
      val hierarchy = FrameHierarchy.fromFrames(dgraph, frames)
      hierarchy.size must_== 1
      hierarchy.head.frame.relation.name must_== "want"
      hierarchy.head.children.size must_== 1
      hierarchy.head.children.head.frame.relation.name must_== "run"
    }
  }
}
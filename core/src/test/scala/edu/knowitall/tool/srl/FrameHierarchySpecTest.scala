package edu.knowitall.tool.srl

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.knowitall.tool.parse.graph.DependencyGraph

@RunWith(classOf[JUnitRunner])
object FrameHierarchySpecTest extends Specification {
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

  {
    val sentence = "Most people make this mistake and over time can become immune to various antibiotics."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("amod(people_NNS_1_5, Most_JJS_0_0); nsubj(make_VBP_2_12, people_NNS_1_5); dobj(make_VBP_2_12, mistake_NN_4_22); cc(make_VBP_2_12, and_CC_5_30); conj(make_VBP_2_12, become_VB_9_48); det(mistake_NN_4_22, this_DT_3_17); pobj(over_IN_6_34, time_NN_7_39); prep(become_VB_9_48, over_IN_6_34); aux(become_VB_9_48, can_MD_8_44); acomp(become_VB_9_48, immune_JJ_10_55); punct(become_VB_9_48, ._._14_84); prep(immune_JJ_10_55, to_IN_11_62); pobj(to_IN_11_62, antibiotics_NNS_13_73); amod(antibiotics_NNS_13_73, various_JJ_12_65)")
      val frames = IndexedSeq(
        "make_2.LV:[A0=people_1, AM-PRR=mistake_4]",
        "mistake_4.01:[A0=people_1, C-V=make_2]",
        "become_9.01:[A1=people_1, AM-TMP=over_6, AM-MOD=can_8, A2=immune_10]") map Frame.deserialize(dgraph)
      val hierarchy = FrameHierarchy.fromFrames(dgraph, frames)
      hierarchy.size must_== 2
    }
  }

  {
    val sentence = "John is beginning to want to blow his nose."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(beginning_VBG_2_8, John_NNP_0_0); aux(beginning_VBG_2_8, is_VBZ_1_5); xcomp(beginning_VBG_2_8, want_VB_4_21); punct(beginning_VBG_2_8, ._._9_42); aux(want_VB_4_21, to_TO_3_18); xcomp(want_VB_4_21, blow_VB_6_29); aux(blow_VB_6_29, to_TO_5_26); dobj(blow_VB_6_29, nose_NN_8_38); poss(nose_NN_8_38, his_PRP$_7_34)")
      val frames = IndexedSeq(
        "begin_2.01:[A0=John_0, A1=want_4]",
        "want_4.01:[A0=John_0, A1=blow_6]",
        "blow_6.09:[A0=John_0, A1=nose_8]") map Frame.deserialize(dgraph)
      val hierarchy = FrameHierarchy.fromFrames(dgraph, frames)
      hierarchy.size must_== 1
      hierarchy.head.height == 3
    }
  }

  {
    val sentence = "John wants to blow his nose and eat corn."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(wants_VBZ_1_6, John_NNP_0_0); xcomp(wants_VBZ_1_6, blow_VB_3_15); punct(wants_VBZ_1_6, ._._9_41); aux(blow_VB_3_15, to_TO_2_12); dobj(blow_VB_3_15, nose_NN_5_24); cc(blow_VB_3_15, and_CC_6_29); conj(blow_VB_3_15, eat_VB_7_33); poss(nose_NN_5_24, his_PRP$_4_20); dobj(eat_VB_7_33, corn_NN_8_37)")
      val frames = IndexedSeq(
        "want_1.01:[A0=John_0, A1=blow_3]",
        "blow_3.09:[A0=John_0, A1=nose_5]",
        "eat_7.01:[A0=John_0, A1=corn_8]") map Frame.deserialize(dgraph)
      val hierarchy = FrameHierarchy.fromFrames(dgraph, frames)
      hierarchy.size must_== 1
      hierarchy.head.height == 2
    }
  }

  {
    val sentence = "John said Fred works on Sunday."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(said_VBD_1_5, John_NNP_0_0); ccomp(said_VBD_1_5, works_VBZ_3_15); punct(said_VBD_1_5, ._._6_30); nsubj(works_VBZ_3_15, Fred_NNP_2_10); prep(works_VBZ_3_15, on_IN_4_21); pobj(on_IN_4_21, Sunday_NNP_5_24)")
      val frames = IndexedSeq(
        "say_1.01:[A0=John_0, A1=works_3]",
        "work_3.01:[A0=Fred_2, A1=on_4]") map Frame.deserialize(dgraph)
      val hierarchy = FrameHierarchy.fromFrames(dgraph, frames)
      hierarchy.size must_== 1
      hierarchy.head.height == 2
    }
  }

  {
    val sentence = "Mary said John wants to sleep under a tree."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(said_VBD_1_5, Mary_NNP_0_0); ccomp(said_VBD_1_5, wants_VBZ_3_15); punct(said_VBD_1_5, ._._9_42); nsubj(wants_VBZ_3_15, John_NNP_2_10); xcomp(wants_VBZ_3_15, sleep_VB_5_24); aux(sleep_VB_5_24, to_TO_4_21); prep(sleep_VB_5_24, under_IN_6_30); pobj(under_IN_6_30, tree_NN_8_38); det(tree_NN_8_38, a_DT_7_36)")
      val frames = IndexedSeq(
        "say_1.01:[A0=Mary_0, A1=wants_3]",
        "want_3.01:[A0=John_2, A1=sleep_5]",
        "sleep_5.01:[A0=John_2, AM-LOC=under_6]") map Frame.deserialize(dgraph)
      val hierarchy = FrameHierarchy.fromFrames(dgraph, frames)
      hierarchy.head.toString must_== "say.01:[A0=Mary, A1=wants] < want.01:[A0=John, A1=sleep] < sleep.01:[A0=John, AM-LOC=under]"
    }
  }

  {
    val sentence = "Montevideo is the capital of Uruguay and is situated where the river Rio de la Plata flows into the South Atlantic ."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(is_VBZ_1_11, Montevideo_NNP_0_0); attr(is_VBZ_1_11, capital_NN_3_18); cc(is_VBZ_1_11, and_CC_6_37); conj(is_VBZ_1_11, situated_VBN_8_44); punct(is_VBZ_1_11, ._._21_115); det(capital_NN_3_18, the_DT_2_14); prep(capital_NN_3_18, of_IN_4_26); pobj(of_IN_4_26, Uruguay_NNP_5_29); auxpass(situated_VBN_8_44, is_VBZ_7_41); advcl(situated_VBN_8_44, flows_VBZ_16_85); det(river_NN_11_63, the_DT_10_59); appos(river_NN_11_63, Plata_NNP_15_79); nn(Plata_NNP_15_79, Rio_NNP_12_69); nn(Plata_NNP_15_79, de_NNP_13_73); nn(Plata_NNP_15_79, la_NNP_14_76); advmod(flows_VBZ_16_85, where_WRB_9_53); nsubj(flows_VBZ_16_85, river_NN_11_63); prep(flows_VBZ_16_85, into_IN_17_91); pobj(into_IN_17_91, Atlantic_NNP_20_106); det(Atlantic_NNP_20_106, the_DT_18_96); nn(Atlantic_NNP_20_106, South_NNP_19_100)")
      val frames = IndexedSeq(
        "be_1.01:[A1=Montevideo_0, A2=capital_3]",
        "situate_8.01:[A1=Montevideo_0]",
        "flow_16.01:[R-AM-LOC=where_9, A1=river_11, AM-DIR=into_17]") map Frame.deserialize(dgraph)
      val hierarchy = FrameHierarchy.fromFrames(dgraph, frames)
      hierarchy.map(_.toString) must haveTheSameElementsAs(Seq("be.01:[A1=Montevideo, A2=capital]", "flow.01:[R-AM-LOC=where, A1=river, AM-DIR=into]", "situate.01:[A1=Montevideo]"))
    }
  }
}
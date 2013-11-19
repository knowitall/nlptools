package edu.knowitall.tool.srl

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import edu.knowitall.tool.parse.graph.DependencyGraph

@RunWith(classOf[JUnitRunner])
object FrameSpecTest extends Specification {
  case class SerializationTest(val dgraphString: String, val frameString: String) {
    def dgraph = DependencyGraph.stringFormat.read(dgraphString)
    def frame(dgraph: DependencyGraph) = Frame.deserialize(dgraph)(frameString)
  }

  val serializationTests = List(
      SerializationTest("nsubj(flew_VBD_1_10, MichaJohn_NNP_0_0); prep(flew_VBD_1_10, from_IN_2_15); prep(flew_VBD_1_10, to_IN_4_27); pobj(from_IN_2_15, Europe_NNP_3_20)", "fly_1.01:[A1=MichaJohn_0, AM_DIR=from_2, AM_DIR=to_4]"),
      SerializationTest("nsubj(turned_VBD_1_8, Michael_NNP_0_0); dobj(turned_VBD_1_8, light_NN_3_19); prt(turned_VBD_1_8, off_RP_4_25); punct(turned_VBD_1_8, ._._5_28); det(light_NN_3_19, the_DT_2_15)", "turn_1.01:[A0=Michael_0, A1=light_3, C-V=off_4]"),
      SerializationTest("nsubj(is_VBZ_1_3, It_PRP_0_0); acomp(is_VBZ_1_3, possible_JJ_2_6); ccomp(is_VBZ_1_3, fall_VB_7_36); punct(is_VBZ_1_3, ._._23_126); det(plan_NN_5_25, this_DT_4_20); complm(fall_VB_7_36, that_IN_3_15); nsubj(fall_VB_7_36, plan_NN_5_25); aux(fall_VB_7_36, could_MD_6_30); advmod(fall_VB_7_36, apart_RB_8_41); punct(fall_VB_7_36, ,_,_9_47); advcl(fall_VB_7_36, have_VBP_11_52); mark(have_VBP_11_52, as_IN_10_49); dobj(have_VBP_11_52, plans_NNS_14_74); amod(plans_NNS_14_74, previous_JJ_12_57); nn(plans_NNS_14_74, Boehner_NNP_13_66); infmod(plans_NNS_14_74, avoid_VB_16_83); aux(avoid_VB_16_83, to_TO_15_80); cc(avoid_VB_16_83, or_CC_17_89); conj(avoid_VB_16_83, deal_VB_18_92); prep(deal_VB_18_92, with_IN_19_97); pobj(with_IN_19_97, shutdown_NN_22_117); det(shutdown_NN_22_117, the_DT_20_102); nn(shutdown_NN_22_117, government_NN_21_106)", "avoid_16.01:[]"),
      SerializationTest("det(hazard_NN_3_15, a_DT_1_5); amod(hazard_NN_3_15, natural_JJ_2_7); advmod(result_VB_6_31, Thus_RB_0_0); nsubj(result_VB_6_31, hazard_NN_3_15); aux(result_VB_6_31, will_MD_4_22); neg(result_VB_6_31, not_RB_5_27); prep(result_VB_6_31, in_IN_7_38); prep(result_VB_6_31, without_IN_13_69); pobj(in_IN_7_38, disaster_NN_10_51); det(disaster_NN_10_51, a_DT_8_41); amod(disaster_NN_10_51, natural_JJ_9_43); prep(disaster_NN_10_51, in_IN_11_60); pobj(in_IN_11_60, areas_NNS_12_63); pobj(without_IN_13_69, vulnerability_NN_14_77); advcl(g._VBD_17_95, result_VB_6_31); punct(g._VBD_17_95, ,_,_15_90); nsubj(g._VBD_17_95, e._NNP_16_92); dobj(g._VBD_17_95, earthquakes_NNS_19_105); prep(g._VBD_17_95, in_IN_20_117); punct(g._VBD_17_95, ._._23_137); amod(earthquakes_NNS_19_105, strong_JJ_18_98); pobj(in_IN_20_117, areas_NNS_22_132); amod(areas_NNS_22_132, uninhabited_JJ_21_120)", "g._17.01:[A0=e._16, A1=earthquakes_19, AM-LOC=in_20]")
  )

  for (test <- serializationTests) {
    (test + " deserializes ok") in {
      val dgraph = test.dgraph
      val frame = test.frame(dgraph)

      frame.serialize must_== test.frameString
    }
  }
}

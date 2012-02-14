package edu.washington.cs.knowitall
package tool
package parse
package graph

import org.junit._
import org.junit.Assert._
import org.specs.Specification
import org.specs.runner.JUnit4
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner

import Graph._

@RunWith(classOf[JUnitSuiteRunner])
class DependencyGraphSpecTest extends JUnit4(DependencyGraphSpec)
object DependencyGraphSpec extends Specification {
  def testNNPOfCollapse = {
    val dgraph = DependencyGraph.deserialize("nsubjpass(born_VBN_5, Graham_NNP_0); nn(California_NNP_3, Southern_NNP_2); prep_of(Graham_NNP_0, California_NNP_3); auxpass(born_VBN_5, was_VBD_4); prep_in(born_VBN_5, Germany_NNP_7)").
            collapseNounGroups().collapseNNPOf

    "prep_of between NNPs are collapsed " in {
      dgraph.graph.vertices.map(_.text) must contain("Graham of Southern California")
    }
  }

  def testNNPOfCollapse2 = {
    // By using this Site you hereby consent to the exclusive personal jurisdiction and venue of the Federal Courts of the United States of America or the State Courts of the State of California located in Los Angeles County , California .
    val dgraph = DependencyGraph.deserialize("pcomp(By_IN_0, using_VBG_1); det(Site_NN_3, this_DT_2); dobj(using_VBG_1, Site_NN_3); nsubj(consent_VB_6, you_PRP_4); advmod(consent_VB_6, hereby_RB_5); rcmod(Site_NN_3, consent_VB_6); det(jurisdiction_NN_11, the_DT_8); amod(jurisdiction_NN_11, exclusive_JJ_9); amod(jurisdiction_NN_11, personal_JJ_10); prep_to(consent_VB_6, jurisdiction_NN_11); prep_to(consent_VB_6, venue_NN_13); conj_and(jurisdiction_NN_11, venue_NN_13); det(Courts_NNP_17, the_DT_15); nn(Courts_NNP_17, Federal_NNP_16); prep_of(venue_NN_13, Courts_NNP_17); det(States_NNP_21, the_DT_19); nn(States_NNP_21, United_NNP_20); prep_of(Courts_NNP_17, States_NNP_21); prep_of(States_NNP_21, America_NNP_23); det(Courts_NNP_27, the_DT_25); nn(Courts_NNP_27, State_NNP_26); prep_of(States_NNP_21, Courts_NNP_27); conj_or(America_NNP_23, Courts_NNP_27); det(State_NNP_30, the_DT_29); prep_of(America_NNP_23, State_NNP_30); prep_of(State_NNP_30, California_NNP_32); partmod(jurisdiction_NN_11, located_VBN_33); nn(County_NNP_37, Los_NNP_35); nn(County_NNP_37, Angeles_NNP_36); prep_in(located_VBN_33, County_NNP_37); punct(County_NNP_37, _,_38); appos(County_NNP_37, California_NNP_39); punct(By_IN_0, ._._40)").
            collapseNounGroups().collapseNNPOf

    "prep_of between NNPs are collapsed " in {
      dgraph.graph.vertices.map(_.text) must contain("Federal Courts")
      dgraph.graph.vertices.map(_.text) must contain("United States of America")
      dgraph.graph.vertices.map(_.text) must contain("State of California")
      dgraph.graph.vertices.map(_.text) must contain("State Courts")
      dgraph.graph.vertices.map(_.text) must contain("Los Angeles County")
    }
  }
  
  def testCollapseDirected = {
    val pickled = "nsubj(seem_VBP_1, I_PRP_0); aux(lived_VBN_4, to_TO_2); aux(lived_VBN_4, have_VB_3); xcomp(seem_VBP_1, lived_VBN_4); det(life_NN_7, a_DT_5); amod(life_NN_7, good_JJ_6); dobj(lived_VBN_4, life_NN_7); punct(seem_VBP_1, ._._8)"
    val dgraph = DependencyGraph.deserialize(pickled)
    val collapsed = dgraph.directedAdjacentCollapse("aux")
    "two aux edges are collapsed into a single node" in {
        collapsed.graph.vertices.map(_.text) must contain("to have lived")
    }
  }

  {
    // pathological nn edges (nn components with non-adjacent nodes)
    val dgraph = DependencyGraph.deserialize("nsubj(relays_VBZ_1, Paul_NNP_0); prep_to(relays_VBZ_1, us_PRP_3); det(will_NN_6, the_DT_4); amod(will_NN_6, good_JJ_5); dobj(relays_VBZ_1, will_NN_6); poss(Father_NNP_10, our_PRP$_8); amod(Father_NNP_10, heavenly_JJ_9); prep_of(will_NN_6, Father_NNP_10); det(Jesus_NNP_14, the_DT_12); nn(Jesus_NNP_14, Lord_NNP_13); prep_of(will_NN_6, Jesus_NNP_14); conj_and(Father_NNP_10, Jesus_NNP_14); punct(relays_VBZ_1, _,_15); xcomp(relays_VBZ_1, saying_VBG_16); punct(BE_VB_19, _,_17); nsubj(BE_VB_19, PEACE_NNP_18); nsubj(LOVE_JJ_25, PEACE_NNP_18); parataxis(saying_VBG_16, BE_VB_19); det(BRETHREN_NN_22, THE_DT_21); prep_to(BE_VB_19, BRETHREN_NN_22); punct(BE_VB_19, _,_23); parataxis(saying_VBG_16, LOVE_JJ_25); conj_and(BE_VB_19, LOVE_JJ_25); nn(FATHER_NNP_32, FAITH_NNP_27); punct(FAITH_NNP_27, _,_28); nn(GOD_NNP_30, FROM_NNP_29); conj(FAITH_NNP_27, GOD_NNP_30); det(FATHER_NNP_32, THE_DT_31); prep_with(LOVE_JJ_25, FATHER_NNP_32); det(CHRIST-Eph_NNP_37, THE_DT_34); nn(CHRIST-Eph_NNP_37, LORD_NNP_35); nn(CHRIST-Eph_NNP_37, JESUS_NNP_36); prep_with(LOVE_JJ_25, CHRIST-Eph_NNP_37); conj_and(FATHER_NNP_32, CHRIST-Eph_NNP_37); num(CHRIST-Eph_NNP_37, 6:23_CD_38); punct(relays_VBZ_1, ._._39)")
  }
  
  {
    val dgraph = DependencyGraph.deserialize("nn(Obama_NNP_3, U.S._NNP_0); nn(Obama_NNP_3, President_NNP_1); nn(Obama_NNP_3, Barack_NNP_2); nsubjpass(elected_VBN_5, Obama_NNP_3); auxpass(elected_VBN_5, was_VBD_4); det(people_NNS_8, the_DT_7); agent(elected_VBN_5, people_NNS_8); punct(elected_VBN_5, ._._9)")
    
	// collapse all noun groups
    val dgraphCollapseAll = dgraph.collapseNounGroups()
    "U.S. President Barack Obama is a single noun group" in {
      dgraphCollapseAll.graph.vertices.map(_.text) must contain("U.S. President Barack Obama")
    }
    
	// collapse some noun groups
    val dgraphCollapseSome = dgraph.collapseNounGroups(List("President"))
    "'U.S.', 'President', and 'Barack Obama' are seperate noun groups" in {
      dgraphCollapseSome.graph.vertices.map(_.text) must contain("U.S.")
      dgraphCollapseSome.graph.vertices.map(_.text) must contain("President")
      dgraphCollapseSome.graph.vertices.map(_.text) must contain("Barack Obama")
    }
  }
  
  "serializes ok without extra nodes" in {
    val pickled = "nsubj(jumped_VBD_1, He_PRP_0); prep_over(jumped_VBD_1, barrier_NN_4); det(barrier_NN_4, the_DT_3)"
    DependencyGraph.deserialize(pickled).serialize must_== pickled
  }
  
  "deserializes fails on garbage" in {
    val pickled = "over_IN_2), nsubj(jumped_VBD_1, He_PRP_0); det(barrier_NN_4, the_DT_3); prep_over(jumped_VBD_1, barrier_NN_4)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes fails on garbage" in {
    val pickled = "ed_VBD_1, He_PRP_0); det(barrier_NN_4, the_DT_3); prep_over(jumped_VBD_1, barrier_NN_4)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes fails on garbage" in {
    val pickled = "(barrier_NN_4, the_DT_3); prep_over(jumped_VBD_1, barrier_NN_4)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes fails on garbage" in {
    val pickled = "; prep_over(jumped_VBD_1, barrier_NN_4)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes ok" in {
    val pickled = "(over_IN_2), nsubj(jumped_VBD_1, He_PRP_0); det(barrier_NN_4, the_DT_3); prep_over(jumped_VBD_1, barrier_NN_4)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with unicode" in {
    val pickled = "(on_IN_1), (by_IN_9), (to_TO_14), (after_IN_17), (of_IN_20), nsubj(words_NNS_7, Peace_NNP_0); nn(^Y_NNP_3, Earth_NNP_2); prep_on(Peace_NNP_0, ^Y_NNP_3); cop(words_NNS_7, were_VBD_4); det(words_NNS_7, the_DT_5); amod(words_NNS_7, first_JJ_6); partmod(words_NNS_7, spoken_VBN_8); det(angel_NN_11, the_DT_10); agent(spoken_VBN_8, angel_NN_11); nsubj(appeared_VBD_13, who_WP_12); rcmod(angel_NN_11, appeared_VBD_13); det(shepherds_NNS_16, the_DT_15); prep_to(appeared_VBD_13, shepherds_NNS_16); det(birth_NN_19, the_DT_18); prep_after(appeared_VBD_13, birth_NN_19); prep_of(birth_NN_19, Jesus_NNP_21); punct(words_NNS_7, ._._22)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with commas" in {
    val pickled = "(''_''_4), (,_,_6), (,_,_8), (with_IN_23), (on_IN_26), (!_._29), det(writing_NN_1, The_DT_0); nsubj(saying_VBG_3, writing_NN_1); aux(saying_VBG_3, is_VBZ_2); dobj(saying_VBG_3, Dad_NNP_5); appos(Dad_NNP_5, mom_NN_7); dep(saying_VBG_3, look_VB_9); nsubj(!_NNS_15, I_PRP_10); cop(!_NNS_15, 'm_VBP_11); amod(!_NNS_15, good_JJ_12); amod(!_NNS_15, left_JJ_13); nn(!_NNS_15, hand_NN_14); nsubj(is_VBZ_17, that_WDT_16); rcmod(!_NNS_15, is_VBZ_17); dobj(telling_VBG_21, what_WP_18); nsubj(telling_VBG_21, you_PRP_19); aux(telling_VBG_21, were_VBD_20); ccomp(is_VBZ_17, telling_VBG_21); dobj(telling_VBG_21, us_PRP_22); det(sunshine_NN_25, a_DT_24); prep_with(telling_VBG_21, sunshine_NN_25); poss(face_NN_28, your_PRP$_27); prep_on(sunshine_NN_25, face_NN_28)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with commas" in {
    val pickled = "(By_IN_0), (to_TO_7), (and_CC_12), (of_IN_14), (of_IN_18), (of_IN_22), (or_CC_24), (of_IN_28), (of_IN_31), (in_IN_34), (,_,_38), (._._40), prepc_by(hereby_VB_5, using_VBG_1); det(Site_NN_3, this_DT_2); dobj(using_VBG_1, Site_NN_3); nsubj(hereby_VB_5, you_PRP_4); dobj(hereby_VB_5, consent_NN_6); det(jurisdiction_NN_11, the_DT_8); amod(jurisdiction_NN_11, exclusive_JJ_9); amod(jurisdiction_NN_11, personal_JJ_10); prep_to(hereby_VB_5, jurisdiction_NN_11); prep_to(hereby_VB_5, venue_NN_13); conj_and(jurisdiction_NN_11, venue_NN_13); det(Courts_NNPS_17, the_DT_15); nn(Courts_NNPS_17, Federal_NNP_16); prep_of(venue_NN_13, Courts_NNPS_17); det(States_NNPS_21, the_DT_19); nn(States_NNPS_21, United_NNP_20); prep_of(Courts_NNPS_17, States_NNPS_21); prep_of(States_NNPS_21, America_NNP_23); det(Courts_NNPS_27, the_DT_25); nn(Courts_NNPS_27, State_NNP_26); prep_of(Courts_NNPS_17, Courts_NNPS_27); conj_or(States_NNPS_21, Courts_NNPS_27); det(State_NN_30, the_DT_29); prep_of(Courts_NNPS_27, State_NN_30); prep_of(State_NN_30, California_NNP_32); partmod(venue_NN_13, located_VBN_33); nn(County_NNP_37, Los_NNP_35); nn(County_NNP_37, Angeles_NNP_36); prep_in(located_VBN_33, County_NNP_37); appos(County_NNP_37, California_NNP_39)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with missing text" in {
    val pickled = "(''_''_4), (_,_6), (_,_8), (with_IN_23), (on_IN_26), (!_._29), det(writing_NN_1, The_DT_0); nsubj(saying_VBG_3, writing_NN_1); aux(saying_VBG_3, is_VBZ_2); dobj(saying_VBG_3, Dad_NNP_5); dep(saying_VBG_3, look_VB_9); appos(Dad_NNP_5, mom_NN_7); nsubj(!_NNS_15, I_PRP_10); cop(!_NNS_15, 'm_VBP_11); amod(!_NNS_15, good_JJ_12); amod(!_NNS_15, left_JJ_13); nn(!_NNS_15, hand_NN_14); rcmod(!_NNS_15, is_VBZ_17); nsubj(is_VBZ_17, that_WDT_16); ccomp(is_VBZ_17, telling_VBG_21); dobj(telling_VBG_21, what_WP_18); nsubj(telling_VBG_21, you_PRP_19); aux(telling_VBG_21, were_VBD_20); dobj(telling_VBG_21, us_PRP_22); prep_with(telling_VBG_21, sunshine_NN_25); det(sunshine_NN_25, a_DT_24); prep_on(sunshine_NN_25, face_NN_28); poss(face_NN_28, your_PRP$_27)"
    val pickledOld = "(''_''_4), (_,_6), (_,_8), (with_IN_23), (on_IN_26), (!_._29), det(writing_NN_1, The_DT_0); nsubj(saying_VBG_3, writing_NN_1); aux(saying_VBG_3, is_VBZ_2); dobj(saying_VBG_3, Dad_NNP_5); appos(Dad_NNP_5, mom_NN_7); dep(saying_VBG_3, look_VB_9); nsubj(!_NNS_15, I_PRP_10); cop(!_NNS_15, 'm_VBP_11); amod(!_NNS_15, good_JJ_12); amod(!_NNS_15, left_JJ_13); nn(!_NNS_15, hand_NN_14); nsubj(is_VBZ_17, that_WDT_16); rcmod(!_NNS_15, is_VBZ_17); dobj(telling_VBG_21, what_WP_18); nsubj(telling_VBG_21, you_PRP_19); aux(telling_VBG_21, were_VBD_20); ccomp(is_VBZ_17, telling_VBG_21); dobj(telling_VBG_21, us_PRP_22); det(sunshine_NN_25, a_DT_24); prep_with(telling_VBG_21, sunshine_NN_25); poss(face_NN_28, your_PRP$_27); prep_on(sunshine_NN_25, face_NN_28)"
    val graph = DependencyGraph.deserialize(pickled)
    val graphOld = DependencyGraph.deserialize(pickledOld)
    
    graph.serialize must_== pickled
    graph.graph must_== graphOld.graph
  }

  testNNPOfCollapse
  testNNPOfCollapse2
  testCollapseDirected
}

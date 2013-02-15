package edu.washington.cs.knowitall
package tool
package parse
package graph

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import collection.immutable.graph.Graph._
import edu.washington.cs.knowitall.tool.stem.Stemmer

@RunWith(classOf[JUnitRunner])
object DependencyGraphSpecTest extends Specification {
  def testNNPOfCollapse = {
    val dgraph = DependencyGraph.deserialize("nsubjpass(born_VBN_5_0, Graham_NNP_0_0); nn(California_NNP_3_0, Southern_NNP_2_0); prep_of(Graham_NNP_0_0, California_NNP_3_0); auxpass(born_VBN_5_0, was_VBD_4_0); prep_in(born_VBN_5_0, Germany_NNP_7_0)").
            collapseNounGroups().collapseNNPOf

    "prep_of between NNPs are collapsed " in {
      dgraph.graph.vertices.map(_.text) must contain("Graham of Southern California")
    }
  }

  def testNNPOfCollapse2 = {
    // By using this Site you hereby consent to the exclusive personal jurisdiction and venue of the Federal Courts of the United States of America or the State Courts of the State of California located in Los Angeles County , California .
    val dgraph = DependencyGraph.deserialize("pcomp(By_IN_0_0, using_VBG_1_0); det(Site_NN_3_0, this_DT_2_0); dobj(using_VBG_1_0, Site_NN_3_0); nsubj(consent_VB_6_0, you_PRP_4_0); advmod(consent_VB_6_0, hereby_RB_5_0); rcmod(Site_NN_3_0, consent_VB_6_0); det(jurisdiction_NN_11_0, the_DT_8_0); amod(jurisdiction_NN_11_0, exclusive_JJ_9_0); amod(jurisdiction_NN_11_0, personal_JJ_10_0); prep_to(consent_VB_6_0, jurisdiction_NN_11_0); prep_to(consent_VB_6_0, venue_NN_13_0); conj_and(jurisdiction_NN_11_0, venue_NN_13_0); det(Courts_NNP_17_0, the_DT_15_0); nn(Courts_NNP_17_0, Federal_NNP_16_0); prep_of(venue_NN_13_0, Courts_NNP_17_0); det(States_NNP_21_0, the_DT_19_0); nn(States_NNP_21_0, United_NNP_20_0); prep_of(Courts_NNP_17_0, States_NNP_21_0); prep_of(States_NNP_21_0, America_NNP_23_0); det(Courts_NNP_27_0, the_DT_25_0); nn(Courts_NNP_27_0, State_NNP_26_0); prep_of(States_NNP_21_0, Courts_NNP_27_0); conj_or(America_NNP_23_0, Courts_NNP_27_0); det(State_NNP_30_0, the_DT_29_0); prep_of(America_NNP_23_0, State_NNP_30_0); prep_of(State_NNP_30_0, California_NNP_32_0); partmod(jurisdiction_NN_11_0, located_VBN_33_0); nn(County_NNP_37_0, Los_NNP_35_0); nn(County_NNP_37_0, Angeles_NNP_36_0); prep_in(located_VBN_33_0, County_NNP_37_0); punct(County_NNP_37_0, _,_38_0); appos(County_NNP_37_0, California_NNP_39_0); punct(By_IN_0_0, ._._40_0)").
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
    val pickled = "nsubj(seem_VBP_1_0, I_PRP_0_0); aux(lived_VBN_4_0, to_TO_2_0); aux(lived_VBN_4_0, have_VB_3_0); xcomp(seem_VBP_1_0, lived_VBN_4_0); det(life_NN_7_0, a_DT_5_0); amod(life_NN_7_0, good_JJ_6_0); dobj(lived_VBN_4_0, life_NN_7_0); punct(seem_VBP_1_0, ._._8_0)"
    val dgraph = DependencyGraph.deserialize(pickled)
    val collapsed = dgraph.directedAdjacentCollapse("aux")
    "two aux edges are collapsed into a single node" in {
        collapsed.graph.vertices.map(_.text) must contain("to have lived")
    }
  }

  {
    // pathological nn edges (nn components with non-adjacent nodes)
    "deserializes ok" in {
      DependencyGraph.deserialize("nsubj(relays_VBZ_1_0, Paul_NNP_0_0); prep_to(relays_VBZ_1_0, us_PRP_3_0); det(will_NN_6_0, the_DT_4_0); amod(will_NN_6_0, good_JJ_5_0); dobj(relays_VBZ_1_0, will_NN_6_0); poss(Father_NNP_10_0, our_PRP$_8_0); amod(Father_NNP_10_0, heavenly_JJ_9_0); prep_of(will_NN_6_0, Father_NNP_10_0); det(Jesus_NNP_14_0, the_DT_12_0); nn(Jesus_NNP_14_0, Lord_NNP_13_0); prep_of(will_NN_6_0, Jesus_NNP_14_0); conj_and(Father_NNP_10_0, Jesus_NNP_14_0); punct(relays_VBZ_1_0, _,_15_0); xcomp(relays_VBZ_1_0, saying_VBG_16_0); punct(BE_VB_19_0, _,_17_0); nsubj(BE_VB_19_0, PEACE_NNP_18_0); nsubj(LOVE_JJ_25_0, PEACE_NNP_18_0); parataxis(saying_VBG_16_0, BE_VB_19_0); det(BRETHREN_NN_22_0, THE_DT_21_0); prep_to(BE_VB_19_0, BRETHREN_NN_22_0); punct(BE_VB_19_0, _,_23_0); parataxis(saying_VBG_16_0, LOVE_JJ_25_0); conj_and(BE_VB_19_0, LOVE_JJ_25_0); nn(FATHER_NNP_32_0, FAITH_NNP_27_0); punct(FAITH_NNP_27_0, _,_28_0); nn(GOD_NNP_30_0, FROM_NNP_29_0); conj(FAITH_NNP_27_0, GOD_NNP_30_0); det(FATHER_NNP_32_0, THE_DT_31_0); prep_with(LOVE_JJ_25_0, FATHER_NNP_32_0); det(CHRIST-Eph_NNP_37_0, THE_DT_34_0); nn(CHRIST-Eph_NNP_37_0, LORD_NNP_35_0); nn(CHRIST-Eph_NNP_37_0, JESUS_NNP_36_0); prep_with(LOVE_JJ_25_0, CHRIST-Eph_NNP_37_0); conj_and(FATHER_NNP_32_0, CHRIST-Eph_NNP_37_0); num(CHRIST-Eph_NNP_37_0, 6:23_CD_38_0); punct(relays_VBZ_1_0, ._._39_0)") must not(throwA[Exception])
    }
  }
  
  {
    val dgraph = DependencyGraph.deserialize("nn(Obama_NNP_3_0, U.S._NNP_0_0); nn(Obama_NNP_3_0, President_NNP_1_0); nn(Obama_NNP_3_0, Barack_NNP_2_0); nsubjpass(elected_VBN_5_0, Obama_NNP_3_0); auxpass(elected_VBN_5_0, was_VBD_4_0); det(people_NNS_8_0, the_DT_7_0); agent(elected_VBN_5_0, people_NNS_8_0); punct(elected_VBN_5_0, ._._9_0)")
    
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
    val pickled = "nsubj(jumped_VBD_1_0, He_PRP_0_0); prep_over(jumped_VBD_1_0, barrier_NN_4_0); det(barrier_NN_4_0, the_DT_3_0)"
    DependencyGraph.deserialize(pickled).serialize must_== pickled
  }
  
  "serializes ok without dependencies" in {
    val pickled = "(over_IN_2_0)"
    DependencyGraph.deserialize(pickled).serialize must_== pickled
  }
  
  "serializes ok without dependencies" in {
    val pickled = "(over_IN_2_0); (through_IN_3_0)"
    DependencyGraph.deserialize(pickled).serialize must_== pickled
  }
  
  "deserializes fails on garbage" in {
    val pickled = "over_IN_2_0); nsubj(jumped_VBD_1_0, He_PRP_0_0); det(barrier_NN_4_0, the_DT_3_0); prep_over(jumped_VBD_1_0, barrier_NN_4_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes fails on garbage" in {
    val pickled = "ed_VBD_1_0, He_PRP_0_0); det(barrier_NN_4_0, the_DT_3_0); prep_over(jumped_VBD_1_0, barrier_NN_4_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes fails on garbage" in {
    val pickled = "(barrier_NN_4_0, the_DT_3_0); prep_over(jumped_VBD_1_0, barrier_NN_4_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes fails on garbage" in {
    val pickled = "; prep_over(jumped_VBD_1_0, barrier_NN_4_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException]
  }
  
  "deserializes ok" in {
    val pickled = "(over_IN_2_0); nsubj(jumped_VBD_1_0, He_PRP_0_0); det(barrier_NN_4_0, the_DT_3_0); prep_over(jumped_VBD_1_0, barrier_NN_4_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with unicode" in {
    val pickled = "(on_IN_1_0); (by_IN_9_0); (to_TO_14_0); (after_IN_17_0); (of_IN_20_0); nsubj(words_NNS_7_0, Peace_NNP_0_0); nn(^Y_NNP_3_0, Earth_NNP_2_0); prep_on(Peace_NNP_0_0, ^Y_NNP_3_0); cop(words_NNS_7_0, were_VBD_4_0); det(words_NNS_7_0, the_DT_5_0); amod(words_NNS_7_0, first_JJ_6_0); partmod(words_NNS_7_0, spoken_VBN_8_0); det(angel_NN_11_0, the_DT_10_0); agent(spoken_VBN_8_0, angel_NN_11_0); nsubj(appeared_VBD_13_0, who_WP_12_0); rcmod(angel_NN_11_0, appeared_VBD_13_0); det(shepherds_NNS_16_0, the_DT_15_0); prep_to(appeared_VBD_13_0, shepherds_NNS_16_0); det(birth_NN_19_0, the_DT_18_0); prep_after(appeared_VBD_13_0, birth_NN_19_0); prep_of(birth_NN_19_0, Jesus_NNP_21_0); punct(words_NNS_7_0, ._._22_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with commas" in {
    val pickled = "(''_''_4_0); (,_,_6_0); (,_,_8_0); (with_IN_23_0); (on_IN_26_0); (!_._29_0); det(writing_NN_1_0, The_DT_0_0); nsubj(saying_VBG_3_0, writing_NN_1_0); aux(saying_VBG_3_0, is_VBZ_2_0); dobj(saying_VBG_3_0, Dad_NNP_5_0); appos(Dad_NNP_5_0, mom_NN_7_0); dep(saying_VBG_3_0, look_VB_9_0); nsubj(!_NNS_15_0, I_PRP_10_0); cop(!_NNS_15_0, 'm_VBP_11_0); amod(!_NNS_15_0, good_JJ_12_0); amod(!_NNS_15_0, left_JJ_13_0); nn(!_NNS_15_0, hand_NN_14_0); nsubj(is_VBZ_17_0, that_WDT_16_0); rcmod(!_NNS_15_0, is_VBZ_17_0); dobj(telling_VBG_21_0, what_WP_18_0); nsubj(telling_VBG_21_0, you_PRP_19_0); aux(telling_VBG_21_0, were_VBD_20_0); ccomp(is_VBZ_17_0, telling_VBG_21_0); dobj(telling_VBG_21_0, us_PRP_22_0); det(sunshine_NN_25_0, a_DT_24_0); prep_with(telling_VBG_21_0, sunshine_NN_25_0); poss(face_NN_28_0, your_PRP$_27_0); prep_on(sunshine_NN_25_0, face_NN_28_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with commas" in {
    val pickled = "(By_IN_0_0); (to_TO_7_0); (and_CC_12_0); (of_IN_14_0); (of_IN_18_0); (of_IN_22_0); (or_CC_24_0); (of_IN_28_0); (of_IN_31_0); (in_IN_34_0); (,_,_38_0); (._._40_0); prepc_by(hereby_VB_5_0, using_VBG_1_0); det(Site_NN_3_0, this_DT_2_0); dobj(using_VBG_1_0, Site_NN_3_0); nsubj(hereby_VB_5_0, you_PRP_4_0); dobj(hereby_VB_5_0, consent_NN_6_0); det(jurisdiction_NN_11_0, the_DT_8_0); amod(jurisdiction_NN_11_0, exclusive_JJ_9_0); amod(jurisdiction_NN_11_0, personal_JJ_10_0); prep_to(hereby_VB_5_0, jurisdiction_NN_11_0); prep_to(hereby_VB_5_0, venue_NN_13_0); conj_and(jurisdiction_NN_11_0, venue_NN_13_0); det(Courts_NNPS_17_0, the_DT_15_0); nn(Courts_NNPS_17_0, Federal_NNP_16_0); prep_of(venue_NN_13_0, Courts_NNPS_17_0); det(States_NNPS_21_0, the_DT_19_0); nn(States_NNPS_21_0, United_NNP_20_0); prep_of(Courts_NNPS_17_0, States_NNPS_21_0); prep_of(States_NNPS_21_0, America_NNP_23_0); det(Courts_NNPS_27_0, the_DT_25_0); nn(Courts_NNPS_27_0, State_NNP_26_0); prep_of(Courts_NNPS_17_0, Courts_NNPS_27_0); conj_or(States_NNPS_21_0, Courts_NNPS_27_0); det(State_NN_30_0, the_DT_29_0); prep_of(Courts_NNPS_27_0, State_NN_30_0); prep_of(State_NN_30_0, California_NNP_32_0); partmod(venue_NN_13_0, located_VBN_33_0); nn(County_NNP_37_0, Los_NNP_35_0); nn(County_NNP_37_0, Angeles_NNP_36_0); prep_in(located_VBN_33_0, County_NNP_37_0); appos(County_NNP_37_0, California_NNP_39_0)"
    DependencyGraph.deserialize(pickled) must throwA[DependencyGraph.SerializationException].not
  }
  
  "deserializes ok with missing text" in {
    val pickled = "(''_''_4_0); (_,_6_0); (_,_8_0); (with_IN_23_0); (on_IN_26_0); (!_._29_0); det(writing_NN_1_0, The_DT_0_0); nsubj(saying_VBG_3_0, writing_NN_1_0); aux(saying_VBG_3_0, is_VBZ_2_0); dobj(saying_VBG_3_0, Dad_NNP_5_0); dep(saying_VBG_3_0, look_VB_9_0); appos(Dad_NNP_5_0, mom_NN_7_0); nsubj(!_NNS_15_0, I_PRP_10_0); cop(!_NNS_15_0, 'm_VBP_11_0); amod(!_NNS_15_0, good_JJ_12_0); amod(!_NNS_15_0, left_JJ_13_0); nn(!_NNS_15_0, hand_NN_14_0); rcmod(!_NNS_15_0, is_VBZ_17_0); nsubj(is_VBZ_17_0, that_WDT_16_0); ccomp(is_VBZ_17_0, telling_VBG_21_0); dobj(telling_VBG_21_0, what_WP_18_0); nsubj(telling_VBG_21_0, you_PRP_19_0); aux(telling_VBG_21_0, were_VBD_20_0); dobj(telling_VBG_21_0, us_PRP_22_0); prep_with(telling_VBG_21_0, sunshine_NN_25_0); det(sunshine_NN_25_0, a_DT_24_0); prep_on(sunshine_NN_25_0, face_NN_28_0); poss(face_NN_28_0, your_PRP$_27_0)"
    val pickledOld = "(''_''_4_0); (_,_6_0); (_,_8_0); (with_IN_23_0); (on_IN_26_0); (!_._29_0); det(writing_NN_1_0, The_DT_0_0); nsubj(saying_VBG_3_0, writing_NN_1_0); aux(saying_VBG_3_0, is_VBZ_2_0); dobj(saying_VBG_3_0, Dad_NNP_5_0); appos(Dad_NNP_5_0, mom_NN_7_0); dep(saying_VBG_3_0, look_VB_9_0); nsubj(!_NNS_15_0, I_PRP_10_0); cop(!_NNS_15_0, 'm_VBP_11_0); amod(!_NNS_15_0, good_JJ_12_0); amod(!_NNS_15_0, left_JJ_13_0); nn(!_NNS_15_0, hand_NN_14_0); nsubj(is_VBZ_17_0, that_WDT_16_0); rcmod(!_NNS_15_0, is_VBZ_17_0); dobj(telling_VBG_21_0, what_WP_18_0); nsubj(telling_VBG_21_0, you_PRP_19_0); aux(telling_VBG_21_0, were_VBD_20_0); ccomp(is_VBZ_17_0, telling_VBG_21_0); dobj(telling_VBG_21_0, us_PRP_22_0); det(sunshine_NN_25_0, a_DT_24_0); prep_with(telling_VBG_21_0, sunshine_NN_25_0); poss(face_NN_28_0, your_PRP$_27_0); prep_on(sunshine_NN_25_0, face_NN_28_0)"
    val graph = DependencyGraph.deserialize(pickled)
    val graphOld = DependencyGraph.deserialize(pickledOld)
    
    graph.serialize must_== pickled
    graph.graph must_== graphOld.graph
  }
  
  "deserialize text with correct offsets" in {
    val pickled = "(._._5_26); nsubj(here_RB_3_14, Michael_NNP_0_0); cop(here_RB_3_14, is_VBZ_1_8); neg(here_RB_3_14, n't_RB_2_10); advmod(here_RB_3_14, anymore_RB_4_19)"
    val graph = DependencyGraph.deserialize(pickled)
    
    graph.text must_== "Michael isn't here anymore."
  }
  
  "serializes to CONLL" in {
    implicit val stemmer = new Stemmer {
      def stem(word: String) = word.toLowerCase
    }
    val pickled = "nsubj(wanted_VBD_1_2, I_PRP_0_0); xcomp(wanted_VBD_1_2, go_VB_3_12); aux(go_VB_3_12, to_TO_2_9); prep(go_VB_3_12, to_TO_4_15); pobj(to_TO_4_15, store_NN_6_22); det(store_NN_6_22, the_DT_5_18); prep(store_NN_6_22, for_IN_7_28); pobj(for_IN_7_28, cream_NN_10_41); det(cream_NN_10_41, some_DT_8_32); nn(cream_NN_10_41, ice_NN_9_37)"
    val graph = DependencyGraph.deserialize(pickled)
    graph.toCONLL must_==("""1	I	i	PRP	_	2	nsubj
2	wanted	wanted	VBD	_	0	root
3	to	to	TO	_	4	aux
4	go	go	VB	_	2	xcomp
5	to	to	TO	_	4	prep
6	the	the	DT	_	7	det
7	store	store	NN	_	5	pobj
8	for	for	IN	_	7	prep
9	some	some	DT	_	11	det
10	ice	ice	NN	_	11	nn
11	cream	cream	NN	_	8	pobj""")
	}

  "deserializes from CONLL" in {
    implicit val stemmer = new Stemmer {
      def stem(word: String) = word.toLowerCase
    }
    val pickled = "nsubj(wanted_VBD_1_2, I_PRP_0_0); xcomp(wanted_VBD_1_2, go_VB_3_12); aux(go_VB_3_12, to_TO_2_9); prep(go_VB_3_12, to_TO_4_15); pobj(to_TO_4_15, store_NN_6_22); det(store_NN_6_22, the_DT_5_18); prep(store_NN_6_22, for_IN_7_28); pobj(for_IN_7_28, cream_NN_10_41); det(cream_NN_10_41, some_DT_8_32); nn(cream_NN_10_41, ice_NN_9_37)"
    val graph = DependencyGraph.deserialize(pickled)
    val newg = DependencyGraph.fromCONLL(graph.toCONLL)
    DependencyGraph.fromCONLL(graph.toCONLL) must_== graph
  }

  testNNPOfCollapse
  testNNPOfCollapse2
  testCollapseDirected
}

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
  def testPrepositionInferral = {
    val dependencies = Dependencies.deserialize("nsubj(son_NN_3, He_PRP_0); cop(son_NN_3, was_VBD_1); det(son_NN_3, the_DT_2); nn(Bell_NNP_6, Graham_NNP_5); prep_of(son_NN_3, Bell_NNP_6); punct(Bell_NNP_6, ,_,_7); nsubjpass(born_VBN_10, who_WP_8); auxpass(born_VBN_10, was_VBD_9); rcmod(Bell_NNP_6, born_VBN_10); prep_in(born_VBN_10, Germany_NNP_12); punct(son_NN_3, ._._13)")
    val dgraph = DependencyGraph(dependencies)

    "prepositions are inferred " in {
      dgraph.nodes.find(_.text == "of") must beSome[DependencyNode]
      dgraph.nodes.find(_.text == "in") must beSome[DependencyNode]
    }
  }

  def testNNPOfCollapse = {
    val dependencies = Dependencies.deserialize("nsubjpass(born_VBN_5, Graham_NNP_0); nn(California_NNP_3, Southern_NNP_2); prep_of(Graham_NNP_0, California_NNP_3); auxpass(born_VBN_5, was_VBD_4); prep_in(born_VBN_5, Germany_NNP_7)")
    val dgraph = DependencyGraph(dependencies).normalize

    "prep_of between NNPs are collapsed " in {
      dgraph.graph.vertices.map(_.text) must contain("Graham of Southern California")
    }
  }

  def testNNPOfCollapse2 = {
    // By using this Site you hereby consent to the exclusive personal jurisdiction and venue of the Federal Courts of the United States of America or the State Courts of the State of California located in Los Angeles County , California .
    val dependencies = Dependencies.deserialize("pcomp(By_IN_0, using_VBG_1); det(Site_NN_3, this_DT_2); dobj(using_VBG_1, Site_NN_3); nsubj(consent_VB_6, you_PRP_4); advmod(consent_VB_6, hereby_RB_5); rcmod(Site_NN_3, consent_VB_6); det(jurisdiction_NN_11, the_DT_8); amod(jurisdiction_NN_11, exclusive_JJ_9); amod(jurisdiction_NN_11, personal_JJ_10); prep_to(consent_VB_6, jurisdiction_NN_11); prep_to(consent_VB_6, venue_NN_13); conj_and(jurisdiction_NN_11, venue_NN_13); det(Courts_NNP_17, the_DT_15); nn(Courts_NNP_17, Federal_NNP_16); prep_of(venue_NN_13, Courts_NNP_17); det(States_NNP_21, the_DT_19); nn(States_NNP_21, United_NNP_20); prep_of(Courts_NNP_17, States_NNP_21); prep_of(States_NNP_21, America_NNP_23); det(Courts_NNP_27, the_DT_25); nn(Courts_NNP_27, State_NNP_26); prep_of(States_NNP_21, Courts_NNP_27); conj_or(America_NNP_23, Courts_NNP_27); det(State_NNP_30, the_DT_29); prep_of(America_NNP_23, State_NNP_30); prep_of(State_NNP_30, California_NNP_32); partmod(jurisdiction_NN_11, located_VBN_33); nn(County_NNP_37, Los_NNP_35); nn(County_NNP_37, Angeles_NNP_36); prep_in(located_VBN_33, County_NNP_37); punct(County_NNP_37, _,_38); appos(County_NNP_37, California_NNP_39); punct(By_IN_0, ._._40)")
    val dgraph = DependencyGraph(dependencies).normalize

    "prep_of between NNPs are collapsed " in {
      dgraph.graph.vertices.map(_.text) must contain("Federal Courts")
      dgraph.graph.vertices.map(_.text) must contain("United States of America")
      dgraph.graph.vertices.map(_.text) must contain("State of California")
      dgraph.graph.vertices.map(_.text) must contain("State Courts")
      dgraph.graph.vertices.map(_.text) must contain("Los Angeles County")
    }
  }

  {
    // pathological nn edges (nn components with non-adjacent nodes)
    val dependencies = Dependencies.deserialize("nsubj(relays_VBZ_1, Paul_NNP_0); prep_to(relays_VBZ_1, us_PRP_3); det(will_NN_6, the_DT_4); amod(will_NN_6, good_JJ_5); dobj(relays_VBZ_1, will_NN_6); poss(Father_NNP_10, our_PRP$_8); amod(Father_NNP_10, heavenly_JJ_9); prep_of(will_NN_6, Father_NNP_10); det(Jesus_NNP_14, the_DT_12); nn(Jesus_NNP_14, Lord_NNP_13); prep_of(will_NN_6, Jesus_NNP_14); conj_and(Father_NNP_10, Jesus_NNP_14); punct(relays_VBZ_1, _,_15); xcomp(relays_VBZ_1, saying_VBG_16); punct(BE_VB_19, _,_17); nsubj(BE_VB_19, PEACE_NNP_18); nsubj(LOVE_JJ_25, PEACE_NNP_18); parataxis(saying_VBG_16, BE_VB_19); det(BRETHREN_NN_22, THE_DT_21); prep_to(BE_VB_19, BRETHREN_NN_22); punct(BE_VB_19, _,_23); parataxis(saying_VBG_16, LOVE_JJ_25); conj_and(BE_VB_19, LOVE_JJ_25); nn(FATHER_NNP_32, FAITH_NNP_27); punct(FAITH_NNP_27, _,_28); nn(GOD_NNP_30, FROM_NNP_29); conj(FAITH_NNP_27, GOD_NNP_30); det(FATHER_NNP_32, THE_DT_31); prep_with(LOVE_JJ_25, FATHER_NNP_32); det(CHRIST-Eph_NNP_37, THE_DT_34); nn(CHRIST-Eph_NNP_37, LORD_NNP_35); nn(CHRIST-Eph_NNP_37, JESUS_NNP_36); prep_with(LOVE_JJ_25, CHRIST-Eph_NNP_37); conj_and(FATHER_NNP_32, CHRIST-Eph_NNP_37); num(CHRIST-Eph_NNP_37, 6:23_CD_38); punct(relays_VBZ_1, ._._39)")
    val dgraph = DependencyGraph(dependencies).normalize
  }
  
  {
    val dependencies = Dependencies.deserialize("nn(Obama_NNP_3, U.S._NNP_0); nn(Obama_NNP_3, President_NNP_1); nn(Obama_NNP_3, Barack_NNP_2); nsubjpass(elected_VBN_5, Obama_NNP_3); auxpass(elected_VBN_5, was_VBD_4); det(people_NNS_8, the_DT_7); agent(elected_VBN_5, people_NNS_8); punct(elected_VBN_5, ._._9)")
	// collapse all noun groups
    val dgraphCollapseAll = DependencyGraph(dependencies).collapseNounGroups()
    "U.S. President Barack Obama is a single noun group" in {
      dgraphCollapseAll.graph.vertices.map(_.text) must contain("U.S. President Barack Obama")
    }
    
	// collapse some noun groups
    val dgraphCollapseSome = DependencyGraph(dependencies).collapseNounGroups(List("President"))
    "'U.S.', 'President', and 'Barack Obama' are seperate noun groups" in {
      dgraphCollapseSome.graph.vertices.map(_.text) must contain("U.S.")
      dgraphCollapseSome.graph.vertices.map(_.text) must contain("President")
      dgraphCollapseSome.graph.vertices.map(_.text) must contain("Barack Obama")
    }
  }

  testPrepositionInferral
  testNNPOfCollapse
  testNNPOfCollapse2
}

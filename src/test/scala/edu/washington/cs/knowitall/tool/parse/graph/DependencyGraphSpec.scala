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

    println(dgraph.graph.vertices.mkString(", "))

    "prep_of between NNPs are collapsed " in {
      dgraph.graph.vertices.find(_.text == "Graham of Southern California") must beSome[DependencyNode]
    }
  }

  def testNNPOfCollapse2 = {
    // By using this Site you hereby consent to the exclusive personal jurisdiction and venue of the Federal Courts of the United States of America or the State Courts of the State of California located in Los Angeles County , California .
    val dependencies = Dependencies.deserialize("pcomp(By_IN_0, using_VBG_1); det(Site_NN_3, this_DT_2); dobj(using_VBG_1, Site_NN_3); nsubj(consent_VB_6, you_PRP_4); advmod(consent_VB_6, hereby_RB_5); rcmod(Site_NN_3, consent_VB_6); det(jurisdiction_NN_11, the_DT_8); amod(jurisdiction_NN_11, exclusive_JJ_9); amod(jurisdiction_NN_11, personal_JJ_10); prep_to(consent_VB_6, jurisdiction_NN_11); prep_to(consent_VB_6, venue_NN_13); conj_and(jurisdiction_NN_11, venue_NN_13); det(Courts_NNP_17, the_DT_15); nn(Courts_NNP_17, Federal_NNP_16); prep_of(venue_NN_13, Courts_NNP_17); det(States_NNP_21, the_DT_19); nn(States_NNP_21, United_NNP_20); prep_of(Courts_NNP_17, States_NNP_21); prep_of(States_NNP_21, America_NNP_23); det(Courts_NNP_27, the_DT_25); nn(Courts_NNP_27, State_NNP_26); prep_of(States_NNP_21, Courts_NNP_27); conj_or(America_NNP_23, Courts_NNP_27); det(State_NNP_30, the_DT_29); prep_of(America_NNP_23, State_NNP_30); prep_of(State_NNP_30, California_NNP_32); partmod(jurisdiction_NN_11, located_VBN_33); nn(County_NNP_37, Los_NNP_35); nn(County_NNP_37, Angeles_NNP_36); prep_in(located_VBN_33, County_NNP_37); punct(County_NNP_37, _,_38); appos(County_NNP_37, California_NNP_39); punct(By_IN_0, ._._40)")
    val dgraph = DependencyGraph(dependencies).normalize

    "prep_of between NNPs are collapsed " in {
      // dgraph.graph.vertices.find(_.text == "Graham of Southern California") must beSome[DependencyNode]
    }
  }


  testPrepositionInferral
  testNNPOfCollapse
  testNNPOfCollapse2
}

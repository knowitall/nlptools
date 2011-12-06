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
  val dependencies = Dependencies.deserialize("nsubj(son_NN_3, He_PRP_0); cop(son_NN_3, was_VBD_1); det(son_NN_3, the_DT_2); nn(Bell_NNP_6, Graham_NNP_5); prep_of(son_NN_3, Bell_NNP_6); punct(Bell_NNP_6, ,_,_7); nsubjpass(born_VBN_10, who_WP_8); auxpass(born_VBN_10, was_VBD_9); rcmod(Bell_NNP_6, born_VBN_10); prep_in(born_VBN_10, Germany_NNP_12); punct(son_NN_3, ._._13)")
  val dgraph = DependencyGraph(dependencies)

  println(dgraph.nodes)

  "prepositions are inferred " in {
    dgraph.nodes.find(_.text == "of") must beSome[DependencyNode]
    dgraph.nodes.find(_.text == "in") must beSome[DependencyNode]
  }
}

package edu.washington.cs.knowitall
package tool
package parse
package pattern

import graph._
import org.specs._
import org.specs.runner._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitSuiteRunner])
class PatternSpecTest extends JUnit4(PatternSpec)
object PatternSpec extends Specification {
  def testMultipathPatternApplication = {
    val pattern = DependencyPattern.deserialize("{arg1} <nsubjpass< {rel:postag=VBN} >prep_in> {arg2}")
    val dgraph = DependencyGraph.deserialize("prep_in(mean_VBP_7, Canada_NNP_1); det(conditions_NNS_6, the_DT_2); amod(conditions_NNS_6, geographic_JJ_3); conj_and(geographic_JJ_3, climatic_JJ_5); amod(conditions_NNS_6, climatic_JJ_5); nsubj(mean_VBP_7, conditions_NNS_6); complm(established_VBN_12, that_IN_8); nsubjpass(established_VBN_12, LBAM_NNP_9); aux(established_VBN_12, could_MD_10); auxpass(established_VBN_12, become_VB_11); ccomp(mean_VBP_7, established_VBN_12); prep_in(established_VBN_12, parts_NNS_14); nn(Columbia_NNP_19, south_NN_16); amod(Columbia_NNP_19, coastal_JJ_17); nn(Columbia_NNP_19, British_NNP_18); prep_of(parts_NNS_14, Columbia_NNP_19); nn(Island_NNP_22, Vancouver_NNP_21); prep_of(parts_NNS_14, Island_NNP_22); conj_and(Columbia_NNP_19, Island_NNP_22); prep_in(established_VBN_12, greenhouses_NNS_27); conj_and(parts_NNS_14, greenhouses_NNS_27); amod(environments_NNS_31, other_JJ_29); amod(environments_NNS_31, protected_JJ_30); conj_and(parts_NNS_14, environments_NNS_31); conj_and(greenhouses_NNS_27, environments_NNS_31); punct(mean_VBP_7, ._._32)")

    pattern.toString should {
      " have two matches" in {
        pattern(dgraph.graph).size must_== 2
      }
    }
  }

  def testPatternSerialization = {
    val patterns = List(
      """{arg1} <nsubj< {rel} >prep_of> {arg2}""",
      """{arg1} <nsubj< {rel} >dobj> {arg2}""",
      """{arg1} <nsubjpass< {slot0} >dep> {rel} >advmod> {arg2}""",
      """{arg1} <nsubj< {rel} >prep_to> {arg2}""",
      """{arg1} >appos> {rel} >prep_of> {arg2}""",
      """{arg1} <nsubjpass< {rel} >prep_in> {arg2}""",
      """{arg1} <nsubj< {rel} >prep_in> {arg2}""",
      """{arg1} <prep_of< {rel} >prep_of> {arg2}""",
      """{arg1} >conj_and> {rel} >prep_of> {arg2}""",
      """{arg1} <nsubj< {rel} >prep_at> {arg2}""",
      """{arg1} <nsubj< {arg2} >cop> {rel}""",
      """{arg1} >partmod> {rel} >prep_in> {arg2}""",
      """{arg1} <nsubjpass< {rel} >dep> {arg2}""",
      """{arg1} <nsubj< {rel} >nsubj> {arg2}""",
      """{arg1} <nsubj< {rel} >dobj> {slot0} >prep_of> {arg2}""",
      """{arg1} <dobj< {rel} >nsubj> {arg2}""",
      """{arg1} <conj_and< {arg2} <prep_of< {rel}""",
      """{arg1} <nsubj< {rel} >prep_on> {arg2}""",
      """{arg1} <nsubj< {rel} >dobj> {slot0} >conj_and> {arg2}""",
      """{arg1} <prep_on< {rel} >prep_on> {arg2}""")
    
    for (patternString <- patterns) {
      patternString should {
        "deserialize correctly" in {
          DependencyPattern.deserialize(patternString).toString must_== patternString
        }
      }
    }
  }

  testPatternSerialization
  testMultipathPatternApplication
}

package edu.knowitall
package tool
package parse
package pattern

import graph._
import tool.stem.IdentityStemmer.instance
import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object PatternSpecTest extends Specification {
  def testMultipathPatternApplication = {
    val pattern = DependencyPattern.deserialize("{arg1} <nsubjpass< {rel:postag=VBN} >prep_in> {arg2}")
    val dgraph = DependencyGraph.stringFormat.read("prep_in(mean_VBP_7_0, Canada_NNP_1_0); det(conditions_NNS_6_0, the_DT_2_0); amod(conditions_NNS_6_0, geographic_JJ_3_0); conj_and(geographic_JJ_3_0, climatic_JJ_5_0); amod(conditions_NNS_6_0, climatic_JJ_5_0); nsubj(mean_VBP_7_0, conditions_NNS_6_0); complm(established_VBN_12_0, that_IN_8_0); nsubjpass(established_VBN_12_0, LBAM_NNP_9_0); aux(established_VBN_12_0, could_MD_10_0); auxpass(established_VBN_12_0, become_VB_11_0); ccomp(mean_VBP_7_0, established_VBN_12_0); prep_in(established_VBN_12_0, parts_NNS_14_0); nn(Columbia_NNP_19_0, south_NN_16_0); amod(Columbia_NNP_19_0, coastal_JJ_17_0); nn(Columbia_NNP_19_0, British_NNP_18_0); prep_of(parts_NNS_14_0, Columbia_NNP_19_0); nn(Island_NNP_22_0, Vancouver_NNP_21_0); prep_of(parts_NNS_14_0, Island_NNP_22_0); conj_and(Columbia_NNP_19_0, Island_NNP_22_0); prep_in(established_VBN_12_0, greenhouses_NNS_27_0); conj_and(parts_NNS_14_0, greenhouses_NNS_27_0); amod(environments_NNS_31_0, other_JJ_29_0); amod(environments_NNS_31_0, protected_JJ_30_0); conj_and(parts_NNS_14_0, environments_NNS_31_0); conj_and(greenhouses_NNS_27_0, environments_NNS_31_0); punct(mean_VBP_7_0, ._._32_0)")

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

  "patterns differ" in {
    DependencyPattern.deserialize("{rel:postag=NNP:regex=president|son} <nn< {arg1} >nn> {arg2}") must_!= DependencyPattern.deserialize("{rel:postag=NN:regex=airline|author|book|brother|candidate|capital|ceo|chair|chairman|champion|chief|city|clone|coach|cofounder|commissioner|consort|cousin|creator|critic|daughter|dean|dictator|director|drummer|economist|editor|emperor|father|founder|friend|god|goddess|governor|graduate|head|home|host|husband|inventor|island|journal|king|lady|language|leader|man|manager|mascot|mayor|member|minister|moon|mother|name|native|newspaper|owner|part|pastor|people|premier|president|prophet|publication|publisher|queen|secretary|sign|son|speaker|star|state|student|subsidiary|suburb|unit|veteran|widow|wife|winner} <nn< {arg1} >nn> {arg2}")
  }
}

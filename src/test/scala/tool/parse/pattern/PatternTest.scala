package edu.washington.cs.knowitall
package tool
package parse
package pattern

import org.specs._
import org.specs.runner._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitSuiteRunner])
class PatternSpecTest extends JUnit4(PatternSpec)
object PatternSpec extends Specification {
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
        Pattern.deserialize(patternString).toString must_== patternString
      }
    }
  }
}
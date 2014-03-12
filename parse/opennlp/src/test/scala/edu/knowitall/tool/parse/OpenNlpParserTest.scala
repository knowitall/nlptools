package edu.knowitall
package tool
package parse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object OpenNlpParserTest extends Specification {
  "constituency parse example sentence" in {
    val text = "This is a test of the OpenNlp parser ."
    val parser = new OpenNlpParser

    val constituency = parser.parse(text)
    constituency.toString must_== "(TOP (S (NP (DT This)) (VP (VBZ is) (NP (NP (DT a) (NN test)) (PP (IN of) (NP (DT the) (NNP OpenNlp) (NN parser))))) (. .)))"
  }
}

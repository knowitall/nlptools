package edu.knowitall
package tool
package parse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object ClearParserTest extends Specification {
  "parse single-word sentence" in {
    val text = "John"
    val parser = new ClearParser()

    val dgraph = parser.dependencyGraph(text)
    DependencyParser.multilineStringFormat.write(dgraph) must_== "John 0 NNP\n\nroot(ROOT-0, John-1)"
  }
}

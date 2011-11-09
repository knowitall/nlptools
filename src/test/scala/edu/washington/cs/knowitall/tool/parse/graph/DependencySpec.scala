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

@RunWith(classOf[JUnitSuiteRunner])
class DependencySpecTest extends JUnit4(DependencySpec)
object DependencySpec extends Specification {
  val nodes = List(
    new DependencyNode("One", "NN", 0),
    new DependencyNode("man", "NN", 1),
    new DependencyNode("fell", "VB", 2),
    new DependencyNode("", ",", 3),
    new DependencyNode("it", "PRP", 4))

  val deps = List(
    new Dependency(nodes(0), nodes(1), "one"),
    new Dependency(nodes(1), nodes(2), "two"),
    new Dependency(nodes(2), nodes(3), "three"),
    new Dependency(nodes(0), nodes(4), "four"))

  "dependencies" should {
    "serialize correcty" in {
      Dependencies.deserialize(Dependencies.serialize(deps)) must_== deps
    }
  }
}
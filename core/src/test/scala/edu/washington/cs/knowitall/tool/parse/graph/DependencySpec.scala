package edu.knowitall
package tool
package parse
package graph

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object DependencySpecTest extends Specification {
  val nodes = List(
    new DependencyNode("One", "NN", 0, -1),
    new DependencyNode("man", "NN", 1, -1),
    new DependencyNode("fell", "VB", 2, -1),
    new DependencyNode("", ",", 3, -1),
    new DependencyNode("it", "PRP", 4, -1))

  val deps = List(
    new Dependency(nodes(0), nodes(1), "one"),
    new Dependency(nodes(0), nodes(4), "four"),
    new Dependency(nodes(1), nodes(2), "two"),
    new Dependency(nodes(2), nodes(3), "three"))

  "dependencies" should {
    "serialize correcty" in {
      Dependencies.deserialize(Dependencies.serialize(deps)) must haveTheSameElementsAs(deps)
    }
  }

  "dependency nodes" should {
    "remove nongraphical characters when serialized" in {
      new DependencyNode("asdf", "NN", 1, 0).serialize must_== "asdf_NN_1_0"
    }
  }
}
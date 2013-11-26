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
    new DependencyNode(0, "One"),
    new DependencyNode(1, "man"),
    new DependencyNode(2, "fell"),
    new DependencyNode(3, "through"),
    new DependencyNode(4, "it"))

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
      DependencyNode.stringFormat.write(new DependencyNode(0, "asdf")) must_== "asdf-0"
    }
  }
}
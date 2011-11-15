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
class GraphSpecTest extends JUnit4(GraphSpec)
object GraphSpec extends Specification {
  val vertices = List(
    "Animal",
    "Mammel",
    "Human",
    "Ape",
    "Cat",
    "Reptile",
    "Lizard"
  ).map(s => (s, s)).toMap

  val edges = List(
    new Edge(vertices("Animal"), vertices("Mammel"), ""),
    new Edge(vertices("Mammel"), vertices("Ape"), ""),
    new Edge(vertices("Mammel"), vertices("Cat"), ""),
    new Edge(vertices("Mammel"), vertices("Human"), ""),
    new Edge(vertices("Animal"), vertices("Reptile"), ""),
    new Edge(vertices("Reptile"), vertices("Lizard"), "")
  )

  val graph = new Graph[String](edges)

  "graph" should {
    "have 6 edges" in {
      graph.edges.size must ==(6)
    }

    "contain {"+edges.mkString(", ")+"}" in {
      graph.edges must haveTheSameElementsAs(edges)
    }

    "contain {" + vertices.values.mkString(", ") + "}" in {
      graph.vertices must haveTheSameElementsAs(vertices.values)
    }
  }

  "vertex Mammel" should {
    val vertex = vertices("Mammel")
    "have 4 neighbors" in {
      graph.neighbors(vertex).size must ==(4)
    }

    val subgraph = List("Mammel", "Ape", "Cat", "Human")
    "have a inferiors of {"+subgraph.mkString(", ")+"}" in {
      graph.inferiors(vertex) must haveTheSameElementsAs(subgraph.map(vertices(_)))
    }
  }

  "vertex Animal" should {
    val vertex = vertices("Animal")
    val neighbors = List("Mammel", "Reptile")
    "have neighbors {"+neighbors.mkString(", ")+"}" in {
      graph.neighbors(vertex) must haveTheSameElementsAs(neighbors.map(vertices(_)))
    }

    "have a inferiors of {"+vertices.keys.mkString(", ")+"}" in {
      graph.inferiors(vertex) must haveTheSameElementsAs(vertices.values)
    }
  }

  val collapse = List("Mammel", "Reptile")
  "graph with {"+collapse.mkString(", ")+"} collapsed" should {
    val mutant = "mutant"
    val elements = (vertices.keys.toSet - "Mammel" - "Reptile").map(vertices(_)) + mutant
    "have elements {" + elements.mkString(", ") + "}" in {
      val collapsed = graph.collapseGroups(Iterable(collapse.map(vertices(_)).toSet))(vertices=>mutant)
      collapsed.vertices must haveTheSameElementsAs(elements)
    }
  }
}

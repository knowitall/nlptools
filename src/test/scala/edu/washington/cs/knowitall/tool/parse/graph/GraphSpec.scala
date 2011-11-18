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

    val neighbors = List("Animal", "Ape", "Cat", "Human")
    "have neighbors {"+neighbors.mkString(", ")+"}" in {
      graph.neighbors(vertex) must haveTheSameElementsAs(neighbors.map(vertices(_)))
    }

    "have neighbors (x=>true) {"+neighbors.mkString(", ")+"}" in {
      graph.neighbors(vertex, x=>true) must haveTheSameElementsAs(neighbors.map(vertices(_)))
    }

    val inferiors = List("Mammel", "Ape", "Cat", "Human")
    "have inferiors {"+inferiors.mkString(", ")+"}" in {
      graph.inferiors(vertex) must haveTheSameElementsAs(inferiors.map(vertices(_)))
    }

    val superiors = List("Animal", "Mammel")
    "have superiors {"+superiors.mkString(", ")+"}" in {
      graph.superiors(vertex) must haveTheSameElementsAs(superiors.map(vertices(_)))
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

@RunWith(classOf[JUnitSuiteRunner])
class GraphSpecLoopTest extends JUnit4(GraphSpecLoop)
object GraphSpecLoop extends Specification {
  val vertices = List(
      "Strange"
  ).map(s => (s, s)).toMap

  val edges = List(
    new Edge(vertices("Strange"), vertices("Strange"), "Loop")
  )

  val graph = new Graph[String](edges)

  "vertex Strange" should {
    "have itself as its only neighbor" in {
      graph.neighbors("Strange") must haveTheSameElementsAs(List("Strange"))
    }
    
    "be connected to only itself" in {
      graph.connected("Strange", x=>true) must haveTheSameElementsAs(List("Strange"))
    }

    "have itself as its only inferior" in {
      graph.inferiors("Strange") must haveTheSameElementsAs(List("Strange"))
    }

    "have itself as its only superior" in {
      graph.superiors("Strange") must haveTheSameElementsAs(List("Strange"))
    }
  }
}

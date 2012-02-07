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
import scala.collection.SortedSet

@RunWith(classOf[JUnitSuiteRunner])
class GraphSpecTest extends JUnit4(GraphSpec)
object GraphSpec extends Specification {
  val vertices = List(
    "Animal",
    "Mammel",
    "Human",
    "Ape",
    "Meower",
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
    new Edge(vertices("Reptile"), vertices("Lizard"), ""),
    new Edge(vertices("Meower"), vertices("Cat"), "")
  )

  val graph = new Graph[String](edges)

  "graph" should {
    "have 7 edges" in {
      graph.edges.size must ==(7)
    }

    "contain {"+edges.mkString(", ")+"}" in {
      graph.edges must haveTheSameElementsAs(edges)
    }

    "contain {" + vertices.values.mkString(", ") + "}" in {
      graph.vertices must haveTheSameElementsAs(vertices.values)
    }
    
    "be connected" in {
      graph.areConnected(graph.vertices) must beTrue
    }
    
    "have a single superior" in {
      graph.superior(graph.vertices) must_== "Animal"
    }
  }
  
  "Mammel, Ape, and Cat" should {
    "have Mammel as the superior" in {
      graph.superior(Set(vertices("Mammel"), vertices("Ape"), vertices("Cat"))) must_== "Mammel"
    }
  }
  
  "Animal, Mammel, Ape, and Cat" should {
    "have Animal as the superior" in {
      graph.superior(Set(vertices("Animal"), vertices("Mammel"), vertices("Ape"), vertices("Cat"))) must_== "Animal"
    }
  }
  
  "Reptile, Mammel, Ape, and Cat" should {
    "have no superior" in {
      graph.superior(Set(vertices("Reptile"), vertices("Mammel"), vertices("Ape"), vertices("Cat"))) must throwA[IllegalArgumentException]
    }
  }
  
  "Mammel, Meower, and Cat" should {
    "have no superior" in {
      graph.superior(Set(vertices("Meower"), vertices("Mammel"), vertices("Cat"))) must throwA[IllegalArgumentException]
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
    
    "neighbor Animal" in {
      graph.neighbors(vertices("Animal"))
      graph.areNeighbors(vertex, vertices("Animal")) must beTrue
    }
    
    "not neighbor Reptile" in {
      graph.areNeighbors(vertex, vertices("Reptile")) must beFalse
    }
    
    "be connected with Animal and Ape" in {
      graph.areConnected(Set(vertex, vertices("Animal"), vertices("Ape"))) must beTrue
    }
    
    "be connected with Animal and Reptile" in {
      graph.areConnected(Set(vertex, vertices("Animal"), vertices("Reptile"))) must beTrue
    }
    
    "not be connected with Animal and Lizard" in {
      graph.areConnected(Set(vertex, vertices("Animal"), vertices("Lizard"))) must beFalse
    }
  }

  "vertex Animal" should {
    val vertex = vertices("Animal")
    val neighbors = List("Mammel", "Reptile")
    "have neighbors {"+neighbors.mkString(", ")+"}" in {
      graph.neighbors(vertex) must haveTheSameElementsAs(neighbors.map(vertices(_)))
    }

    "have inferiors of {"+(vertices.keySet-"Meower").mkString(", ")+"}" in {
      println(graph.inferiors(vertex))
      println(vertices.keySet - "Meower")
      graph.inferiors(vertex) must haveTheSameElementsAs(vertices.values.toSet-vertices("Meower"))
    }
  }

  {
    val collapse = List("Mammel", "Reptile")
    "graph with {" + collapse.mkString(", ") + "} collapsed" should {
      val mutant = "mutant"
      val elements = (vertices.keys.toSet - "Mammel" - "Reptile").map(vertices(_)) + mutant
      "have elements {" + elements.mkString(", ") + "}" in {
        val collapsed = graph.collapse(collapse.map(vertices(_)).toSet)(vertices => mutant)
        collapsed.vertices must haveTheSameElementsAs(elements)
      }
    }
  }

  {
    val collapse = List("Cat", "Ape", "Mammel")
    "graph with {" + collapse.mkString(", ") + "} collapsed" should {
      val mutant = "mutant"
      val elements = (vertices.keys.toSet - "Mammel" - "Cat" - "Ape").map(vertices(_)) + mutant
      val collapsed = graph.collapse(collapse.map(vertices(_)).toSet)(vertices => mutant)
      "have elements {" + elements.mkString(", ") + "}" in {
        collapsed.vertices must haveTheSameElementsAs(elements)
      }
      
      val parents = SortedSet[String]() ++ Iterable("Animal", "Meower")
      val children = SortedSet[String]() + "Human"
      val neighbors = parents ++ children
      val neigbors = parents ++ children
      "have " + neighbors.mkString(", ") + " as neighbors of the collapsed node" in {
        collapsed.neighbors("mutant") must haveTheSameElementsAs(neighbors)
        collapsed.predecessors("mutant") must haveTheSameElementsAs(parents)
        collapsed.successors("mutant") must haveTheSameElementsAs(children)
      }
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

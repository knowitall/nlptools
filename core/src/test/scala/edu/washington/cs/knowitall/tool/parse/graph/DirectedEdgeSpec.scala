package edu.washington.cs.knowitall
package tool
package parse
package graph

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import edu.knowitall.collection.immutable.graph.UpEdge

@RunWith(classOf[JUnitRunner])
object DirectedEdgeSpecTest extends Specification {
  val node1 = new DependencyNode("foo", "nnp", 1, 0)
  val node2 = new DependencyNode("bar", "nnp", 2, 0)
  val dependency = new Dependency(node1, node2, "label")
  val upedge = new UpEdge[DependencyNode](dependency)

  "a DirectedEdge" should {
    "equal itself when flipped twice" in {
      upedge must_== (upedge.flip.flip)
    }
  }
}

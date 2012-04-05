package edu.washington.cs.knowitall
package tool
package parse
package graph

import org.specs.Specification
import org.specs.runner.JUnit4
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import collection.immutable.graph.UpEdge

@RunWith(classOf[JUnitSuiteRunner])
class DirectedEdgeSpecTest extends JUnit4(DirectedEdgeSpec)
object DirectedEdgeSpec extends Specification {
  val node1 = new DependencyNode("foo", "nnp", 1)
  val node2 = new DependencyNode("bar", "nnp", 2)
  val dependency = new Dependency(node1, node2, "label")
  val upedge = new UpEdge[DependencyNode](dependency)
  
  "a DirectedEdge" should {
    "equal itself when flipped twice" in {
      upedge must_== (upedge.flip.flip)
    }
  }
}
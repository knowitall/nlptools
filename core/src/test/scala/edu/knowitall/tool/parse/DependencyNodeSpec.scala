package edu.knowitall
package tool
package parse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import edu.knowitall.tool.parse.graph.DependencyNode

@RunWith(classOf[JUnitRunner])
object DependencyNodeSpec extends Specification {
  "DependencyNode with hyphen round trips through serialization" in {
    val pickledDepNode = "Co-Redemptrix-13"
    val depNode = DependencyNode.stringFormat.read(pickledDepNode)
    val repickled = DependencyNode.stringFormat.write(depNode)

    pickledDepNode must_== repickled
  }
}


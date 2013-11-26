package edu.knowitall
package tool
package chunk

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import edu.knowitall.tool.tokenize._
import edu.knowitall.tool.postag._
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.graph.Dependency

@RunWith(classOf[JUnitRunner])
object DependencySpec extends Specification {
  "Dependency round trips through serialization" in {
    val pickledDep = "det(reflection-9, the-6)"
    val dep = Dependency.stringFormat.read(pickledDep)
    val repickled = Dependency.stringFormat.write(dep)

    pickledDep must_== repickled
  }
}
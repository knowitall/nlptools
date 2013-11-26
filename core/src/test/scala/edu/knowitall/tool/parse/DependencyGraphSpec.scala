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

@RunWith(classOf[JUnitRunner])
object DependencyGraphSpec extends Specification {
  "DependencyGraph round trips through serialization" in {
    val sentence = "John very quickly ran away from the deep blue reflection in the mirror."

    val multilinePickled = """ohn 0 NNP
very 5 RB
quickly 10 RB
ran 18 VBD
away 22 RB
from 27 IN
the 32 DT
deep 36 JJ
blue 41 JJ
reflection 46 NN
in 57 IN
the 60 DT
mirror 64 NN
. 70 .

det(reflection-9, the-6)
advmod(quickly-2, very-1)
prep(reflection-9, in-10)
nsubj(ran-3, John-0)
advmod(ran-3, away-4)
det(mirror-12, the-11)
pobj(in-10, mirror-12)
prep(ran-3, from-5)
amod(reflection-9, blue-8)
advmod(ran-3, quickly-2)
amod(reflection-9, deep-7)
pobj(from-5, reflection-9)"""

    // deserialize and check counts
    val dgraph @ (tokens, graph) = DependencyParser.multilineStringFormat.read(multilinePickled)
    tokens.size must_== 14
    graph.dependencies.size must_== 12

    // reserialize and check match
    val repickled = DependencyParser.multilineStringFormat.write(dgraph)
    multilinePickled must_== repickled
  }
}
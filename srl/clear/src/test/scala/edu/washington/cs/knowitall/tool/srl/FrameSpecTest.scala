package edu.washington.cs.knowitall.tool.srl

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

@RunWith(classOf[JUnitRunner])
object FrameSpecTest extends Specification {
  case class SerializationTest(val dgraphString: String, val frameString: String) {
    def dgraph = DependencyGraph.deserialize(dgraphString)
    def frame(dgraph: DependencyGraph) = Frame.deserialize(dgraph)(frameString)
  }

  val serializationTests = List(
      SerializationTest("nsubj(flew_VBD_1_10, MichaJohn_NNP_0_0); prep(flew_VBD_1_10, from_IN_2_15); prep(flew_VBD_1_10, to_IN_4_27); pobj(from_IN_2_15, Europe_NNP_3_20)", "fly_1.01:[A1=MichaJohn_0, AM_DIR=from_2, AM_DIR=to_4]")
  )

  for (test <- serializationTests) {
    (test + " deserializes ok") in {
      val dgraph = test.dgraph
      val frame = test.frame(dgraph)

      frame.serialize must_== test.frameString
    }
  }
}
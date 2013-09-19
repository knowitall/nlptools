package edu.knowitall.tool.conf

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
object WekaConfidenceFunctionTest extends Specification {

  case class TestObject(x: Double, y: Double, z: Double)

  val unlabeledExamples = Seq.fill(500) {
    TestObject(Random.nextDouble, Random.nextDouble, Random.nextDouble)
  }

  // Label according to a simple boolean function
  val labeledExamples = unlabeledExamples.map { ex =>
    val label = (ex.x + ex.y > 0.5 && ex.z < 0.25)
    Labelled(label, ex)
  }

  val testPositive1 = TestObject(1.0, 1.0, 0.0)
  val testPositive2 = TestObject(0.75, 0.25, 0.1)
  val testNegative1 = TestObject(0.0, 0.0, 1.0)
  val testNegative2 = TestObject(0.2, 0.1, 0.5)

  val fx = Feature.from("x", (ex: TestObject) => ex.x)
  val fy = Feature.from("y", (ex: TestObject) => ex.y)
  val fz = Feature.from("z", (ex: TestObject) => ex.z)
  val noise = Feature.from("random", (x: String) => Random.nextDouble)
  val featureSet = FeatureSet[TestObject, Double](Iterable(fx, fy, fz))

  def testWekaTrainer(name: String, trainer: WekaConfidenceTrainer[TestObject]): Unit = {
    s"a simple $name training scenario" should {

      val conf = trainer.train(labeledExamples)

      "works for easy true" in {
        conf(testPositive1) must be_>(0.5)
      }
      "works for hard true" in {
        conf(testPositive2) must be_>(0.5)
      }
      "works for easy negative" in {
        conf(testNegative1) must be_<=(0.5)
      }
      "works for hard negative" in {
        conf(testNegative2) must be_<=(0.5)
      }
    }
  }

  testWekaTrainer("logistic", new WekaLogisticTrainer(featureSet))

  testWekaTrainer("J48", new WekaJ48Trainer(featureSet))

  testWekaTrainer("REPTree", new WekaREPTreeTrainer(featureSet))

  testWekaTrainer("RandomForest", new WekaRandomForestTrainer(featureSet))
}

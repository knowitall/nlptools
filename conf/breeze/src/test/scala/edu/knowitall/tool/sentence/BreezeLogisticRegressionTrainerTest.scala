package edu.knowitall
package tool
package conf

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object BreezeLogisticRegressionTrainerTest extends Specification {
  "a simple training scenario" should {
    val `true` = Labelled[String](true, "true")
    val `false` = Labelled[String](false, "false")
    val examples = Seq(`true`, `true`, `true`, `false`, `false`, `false`)

    val feature = Feature.from("feature", (x: String) => x == "true")
    val intercept = Feature.from("Intercept", (x: String) => 1.0)
    val featureSet = FeatureSet[String, Double](Iterable(feature, intercept))
    val trainer = new BreezeLogisticRegressionTrainer[String](featureSet)

    val conf = trainer.train(examples)

    "works for true" in {
      conf("true") must be_>(0.5)
    }

    // there is no intercept with L1 so the false case
    // will have a confidence value of 0.5
    "works for false" in {
      conf("false") must be_<=(0.5)
    }
  }
}

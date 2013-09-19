package edu.knowitall
package tool
package conf

import weka.classifiers.Classifier
import weka.core.Instances
import java.io.OutputStream

class WekaConfidenceFunction[E](
  featureSet: FeatureSet[E, Double],
  classifier: Classifier,
  converter: WekaInstanceConverter[E]) extends ConfidenceFunction[E](featureSet) {

  def apply(e: E) = {
    val inst = converter.toUnlabeledInstance(e)
    classifier.distributionForInstance(inst)(1)
  }

  def save(output: OutputStream): Unit = {
    throw new UnsupportedOperationException()
  }
}
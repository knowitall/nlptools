package edu.knowitall
package tool
package conf

import weka.classifiers.AbstractClassifier
import weka.core.Instances
import java.io.OutputStream
import weka.core.SerializationHelper

class WekaConfidenceFunction[E](
  featureSet: FeatureSet[E, Double],
  classifier: AbstractClassifier,
  converter: WekaInstanceConverter[E]) extends ConfidenceFunction[E](featureSet) {

  def apply(e: E) = {
    val inst = converter.toUnlabeledInstance(e)
    classifier.distributionForInstance(inst)(1)
  }

  def save(output: OutputStream): Unit = {
    SerializationHelper.write(output, classifier)
  }
}
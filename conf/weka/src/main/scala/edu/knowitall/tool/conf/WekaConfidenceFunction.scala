package edu.knowitall
package tool
package conf

import weka.classifiers.Classifier
import weka.core.Instances
import java.io.OutputStream

class WekaConfidenceFunction[E](
    featureSet: FeatureSet[E, Double], 
    classifier: Classifier,
    instances: Instances) extends ConfidenceFunction[E](featureSet) {
  
  private val converter = new WekaInstanceConverter[E](featureSet)
  
  def apply(e: E) = {
    val inst = converter.toUnlabeledInstance(instances)(e)
    classifier.distributionForInstance(inst)(1)
  }
  
  def save(output: OutputStream): Unit = {
    throw new UnsupportedOperationException()
  }  
}
package edu.knowitall
package tool
package conf

import weka.core.DenseInstance
import weka.core.Instance
import weka.core.Instances
import weka.core.Attribute
import scala.collection.JavaConversions._
import java.util.ArrayList

/**
 * Wraps the Weka Instances container and provides translation from features of E to Weka Instance objects.
 */
class WekaInstanceCollection[E](val training: Iterable[Labelled[E]], val featureSet: FeatureSet[E, Double]) {

  private val classValues = List("positive", "negative")

  private val classAttr = new Attribute("class", classValues)

  val attributes = {
    val featureAttrs = featureSet.featureNames.map(name => new Attribute(name))
    featureAttrs :+ classAttr
  }

  // Weka requires that Attributes be in an ArrayList
  private val attrsList = {
    val list = new ArrayList[Attribute](attributes.size)
    list.addAll(attributes)
    list
  }

  val trainingInstances = {
    val insts = new Instances("Default training instances", attrsList, 0)
    insts.setClass(classAttr)
    training map toLabeledInstance foreach insts.add
    insts
  }

  def toUnlabeledInstance(item: E): Instance = {
    val attrValues = featureSet.vectorize(item)
    val inst = new DenseInstance(attrValues.size)
    inst.setDataset(trainingInstances)
    attributes.zip(attrValues).foreach { case (attr, value) => inst.setValue(attr, value) }
    inst
  }

  def toLabeledInstance(datum: Labelled[E]): Instance = {
    val label = if (datum.label) 1.0 else 0.0
    val attrValues = featureSet.vectorize(datum.item) :+ label
    val inst = new DenseInstance(attributes.size)
    inst.setDataset(trainingInstances)
    attributes.zip(attrValues).foreach { case (attr, value) => inst.setValue(attr, value) }
    inst
  }


}
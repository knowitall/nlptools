package edu.knowitall
package tool
package conf

import weka.core.DenseInstance
import weka.core.Instance
import weka.core.Instances
import weka.core.Attribute
import scala.collection.JavaConversions._
import java.util.ArrayList

class WekaInstanceConverter[E](featureSet: FeatureSet[E, Double]) {
  
  private val classValues = List("positive", "negative")
  
  private val classAttr = new Attribute("class", classValues)
  
  private val attrs = {
    val featureAttrs = featureSet.featureNames.map(name => new Attribute(name))
    featureAttrs :+ classAttr
  }
  
  private val attrsList = {
    val list = new ArrayList[Attribute](attrs.size)
    attrs foreach list.add
    list
  }
  
  def toUnlabeledInstance(instances: Instances)(e: E): Instance = {
    val attrValues = featureSet.vectorize(e)
    val inst = new DenseInstance(attrValues.size)
    inst.setDataset(instances)
    attrs.zip(attrValues).foreach { case (attr, value) => inst.setValue(attr, value) }
    inst
  }
  
  def toLabeledInstance(instances: Instances)(datum: Labelled[E]): Instance = {
    val label = if (datum.label) 1.0 else 0.0
    val attrValues = featureSet.vectorize(datum.item) :+ label
    val inst = new DenseInstance(attrs.size)
    inst.setDataset(instances)
    attrs.zip(attrValues).foreach { case (attr, value) => inst.setValue(attr, value) }
    inst
  }
  
  def toInstances(training: Iterable[Labelled[E]])  = {
    val insts = new Instances("Default training instances", attrsList, 0)
    insts.setClass(classAttr)
    training map toLabeledInstance(insts) foreach insts.add
    insts
  }
}
package edu.knowitall
package tool
package conf

import weka.core.Instance
import weka.classifiers.AbstractClassifier

/**
 * Subclasses implement newClassifier() to supply a new instance of the appropriate classifier.
 *
 * "options" are passed directly to weka's AbstractClassifier.setOptions
 * See weka docs for more info.
 * See also weka.core.Utils.splitOptions
 */
abstract class WekaConfidenceTrainer[E](features: FeatureSet[E, Double], options: Seq[String])
  extends ConfidenceTrainer[E](features) {

  /**
   * Returns a new instance of the classifier to be trained
   */
  protected def newClassifier(): AbstractClassifier

  def train(training: Iterable[Labelled[E]]): WekaConfidenceFunction[E] = {
    val converter = new WekaInstanceCollection(training, features)
    val classifier = newClassifier()
    classifier.setOptions(options.toArray)
    classifier.buildClassifier(converter.trainingInstances)
    new WekaConfidenceFunction(features, classifier, converter)
  }
}

//
// Convenience classes for training common weka classifiers
//

class WekaLogisticTrainer[E](features: FeatureSet[E, Double], val options: Seq[String] = Nil)
  extends WekaConfidenceTrainer[E](features, options) {

  override def newClassifier() = new weka.classifiers.functions.Logistic()
}

class WekaJ48Trainer[E](features: FeatureSet[E, Double], val options: Seq[String] = Nil)
  extends WekaConfidenceTrainer[E](features, options) {

  override def newClassifier() = new weka.classifiers.trees.J48()
}

class WekaREPTreeTrainer[E](features: FeatureSet[E, Double], val options: Seq[String] = Nil)
  extends WekaConfidenceTrainer[E](features, options) {

  override def newClassifier() = new weka.classifiers.trees.REPTree()
}

class WekaRandomForestTrainer[E](features: FeatureSet[E, Double], val options: Seq[String] = Nil)
  extends WekaConfidenceTrainer[E](features, options) {

  override def newClassifier() = new weka.classifiers.trees.RandomForest()
}
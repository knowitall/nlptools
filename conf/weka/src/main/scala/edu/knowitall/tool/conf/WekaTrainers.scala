package edu.knowitall
package tool
package conf

import weka.core.Instance
import weka.classifiers.Classifier

// TODO: Handle options for each classifier somehow.
abstract class WekaConfidenceTrainer[E](features: FeatureSet[E, Double], options: String = "") extends ConfidenceTrainer[E](features) {
  
  protected def train(converter: WekaInstanceConverter[E]): WekaConfidenceFunction[E]
  
  def train(training: Iterable[Labelled[E]]): WekaConfidenceFunction[E] = {
    val converter = new WekaInstanceConverter(training, features)
    train(converter)
  }
}

class WekaLogisticTrainer[E](features: FeatureSet[E, Double]) extends WekaConfidenceTrainer[E](features) { 

  import weka.classifiers.functions.Logistic
  
  override def train(converter: WekaInstanceConverter[E]): WekaConfidenceFunction[E] = { 
    val logistic = new weka.classifiers.functions.Logistic()
    logistic.buildClassifier(converter.trainingInstances)
    new WekaConfidenceFunction(features, logistic, converter)
  }
}

class WekaJ48Trainer[E](features: FeatureSet[E, Double]) extends WekaConfidenceTrainer[E](features) {
  
  import weka.classifiers.trees.J48 
  
  override def train(converter: WekaInstanceConverter[E]): WekaConfidenceFunction[E] = {
    val j48 = new weka.classifiers.trees.J48()
    j48.buildClassifier(converter.trainingInstances)
    new WekaConfidenceFunction(features, j48, converter)
  }
}

class WekaREPTreeTrainer[E](features: FeatureSet[E, Double]) extends WekaConfidenceTrainer[E](features) {
  
  import weka.classifiers.trees.REPTree 
  
  override def train(converter: WekaInstanceConverter[E]): WekaConfidenceFunction[E] = { 
    val repTree = new weka.classifiers.trees.REPTree()
    repTree.buildClassifier(converter.trainingInstances)
    new WekaConfidenceFunction(features, repTree, converter)
  }
}

class WekaRandomForestTrainer[E](features: FeatureSet[E, Double]) extends WekaConfidenceTrainer[E](features) {
  
  import weka.classifiers.trees.RandomForest
  
  override def train(converter: WekaInstanceConverter[E]): WekaConfidenceFunction[E] = {
    val randomForest = new weka.classifiers.trees.RandomForest()
    randomForest.buildClassifier(converter.trainingInstances)
    new WekaConfidenceFunction(features, randomForest, converter)
  }
}
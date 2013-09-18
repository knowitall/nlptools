package edu.knowitall
package tool
package conf

abstract class WekaConfidenceTrainer[E](features: FeatureSet[E, Double]) extends ConfidenceTrainer[E](features) {
  
  protected val converter = new WekaInstanceConverter(features)
}

class J48Trainer[E](features: FeatureSet[E, Double]) extends WekaConfidenceTrainer[E](features) {
  
  import weka.core.Instance
  import weka.classifiers.Classifier
  import weka.classifiers.trees.J48 
  
  def train(training: Iterable[Labelled[E]]): WekaConfidenceFunction[E] = {
    val instances = converter.toInstances(training) 
    val j48 = new weka.classifiers.trees.J48()
    // -R reduced error pruning
    // -Q 1 random seed
    // -A laplace smoothing probabilities
    val options = Array("-R", "-Q 1", "-A") 
    j48.setOptions(options)
    j48.buildClassifier(instances)
    new WekaConfidenceFunction(features, j48, instances)
  }
}

class REPTreeTrainer[E](features: FeatureSet[E, Double]) extends WekaConfidenceTrainer[E](features) {
  
  import weka.core.Instance
  import weka.classifiers.Classifier
  import weka.classifiers.trees.REPTree 
  
  def train(training: Iterable[Labelled[E]]): WekaConfidenceFunction[E] = {
    val instances = converter.toInstances(training) 
    val repTree = new weka.classifiers.trees.REPTree()
    // -S 1 random seed
    val options = Array("-S 1") 
    repTree.setOptions(options)
    repTree.buildClassifier(instances)
    new WekaConfidenceFunction(features, repTree, instances)
  }
}

class RandomForestTrainer[E](features: FeatureSet[E, Double]) extends WekaConfidenceTrainer[E](features) {
  
  import weka.core.Instance
  import weka.classifiers.Classifier
  import weka.classifiers.trees.RandomForest
  
  def train(training: Iterable[Labelled[E]]): WekaConfidenceFunction[E] = {
    val instances = converter.toInstances(training) 
    val randomForest = new weka.classifiers.trees.RandomForest()
    // -I 10 num trees
    // -K num features
    // -S random seed
    val options = s"-I 10 -K ${features.numFeatures/2} -S 1".split(" ")
    randomForest.setOptions(options)
    randomForest.buildClassifier(instances)
    new WekaConfidenceFunction(features, randomForest, instances)
  }
}
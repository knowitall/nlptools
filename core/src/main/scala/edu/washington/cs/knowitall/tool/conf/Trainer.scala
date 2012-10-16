package edu.washington.cs.knowitall.tool.conf

abstract class Trainer[E, V](val features: FeatureSet[E, V]) {
  val apply = train _
  def train(examples: Iterable[Labelled[E]]): Function[E, V]
}
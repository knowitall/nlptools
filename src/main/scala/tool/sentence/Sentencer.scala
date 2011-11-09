package edu.washington.cs.knowitall
package tool
package sentence

import common.main.LineProcessor

abstract class Sentencer {
  def sentences(document: String): Iterable[String]
}

abstract class SentencerMain
extends LineProcessor {
  def sentencer: Sentencer
  override def process(line: String) = 
    sentencer.sentences(line).zipWithIndex.map { case (sentence, index) => 
      index + ": " + sentence
    }.mkString("\n")
}

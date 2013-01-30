package edu.washington.cs.knowitall
package tool
package segment

import _root_.edu.washington.cs.knowitall.collection.immutable.Interval

/** A sentencer breaks text into sentences.
  */
abstract class Segmenter {
  def apply(document: String) = segment(document)

  def segmentTexts(document: String): Iterable[String]
  def segment(document: String): Iterable[Segment]
}

case class Segment(text: String, offset: Int) {
  def interval = Interval.open(offset, offset + text.length)
  def length = text.length
}

object Segment {
  implicit def asString(segment: Segment): String = segment.text
}

abstract class SegmenterMain
extends LineProcessor("segmenter") {
  def sentencer: Segmenter
  override def process(line: String) =
    sentencer(line).zipWithIndex.map { case (sentence, index) =>
      index + ": " + sentence
    }.mkString("\n")
}

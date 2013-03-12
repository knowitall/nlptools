package edu.knowitall
package tool
package sentence

import spiaotools.SentParDetector

/** A heuristic-based sentencer. */
class PiaoSentencer() extends Sentencer {
  val sentencer = new SentParDetector
  def sentences(document: String) = 
    sentencer.markupRawText(2, document).split("\\n+")
}

object PiaoSentencerMain
extends SentencerMain {
  lazy val sentencer = new PiaoSentencer
}


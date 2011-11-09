package edu.washington.cs.knowitall
package tool
package sentence

import common.main.LineProcessor
import opennlp.tools.sentdetect._

class OpenNlpSentencer(val model: SentenceModel) extends Sentencer {
  val sentencer = new SentenceDetectorME(model)

  def this(modelName: String = "en-sent.bin") =
    this(new SentenceModel(
        classOf[OpenNlpSentencer].getClassLoader.getResourceAsStream(modelName)))

  def sentences(document: String) = sentencer.sentDetect(document)
}

object OpenNlpSentencer
extends SentencerMain {
  lazy val sentencer = new OpenNlpSentencer
}

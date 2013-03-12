package edu.knowitall
package tool
package sentence

import edu.knowitall.tool.segment.Segmenter
import edu.knowitall.tool.segment.SegmenterMain
import opennlp.tools.sentdetect.SentenceDetectorME
import opennlp.tools.sentdetect.SentenceModel
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.segment.Segment

/** A max-ent sentencer. */
class OpenNlpSentencer(val model: SentenceModel) extends Segmenter {
  val sentencer = new SentenceDetectorME(model)

  def this(modelName: String = "en-sent.bin") =
    this(new SentenceModel(
        classOf[OpenNlpSentencer].getClassLoader.getResourceAsStream(modelName)))

  override def segmentTexts(document: String) = {
    sentencer.sentDetect(document)
  }

  def segment(document: String) = {
    val spans = sentencer.sentPosDetect(document)
    spans.map { span =>
      val text = document.substring(span.getStart, span.getEnd)
      val offset = span.getStart()
      Segment(text, offset)
    }
  }
}

object OpenNlpSegmenterMain
extends SegmenterMain {
  lazy val sentencer = new OpenNlpSentencer
}

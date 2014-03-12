package edu.knowitall
package tool
package sentence

import breeze.text.segment.JavaSentenceSegmenter
import edu.knowitall.tool.segment.Segmenter
import edu.knowitall.tool.segment.SegmenterMain
import edu.knowitall.tool.tokenize.Tokenizer
import edu.knowitall.tool.segment.Segment
import edu.knowitall.tool.tokenize.Token

class BreezeSentencer extends Segmenter {
  val sentencer = new JavaSentenceSegmenter()

  override def segmentTexts(document: String) = {
    sentencer(document)
  }

  def segment(document: String) = {
    Tokenizer.computeOffsets(segmentTexts(document), document).map {
      case Token(string, offset) => Segment(string, offset)
    }
  }
}

object BreezeSentencerMain
    extends SegmenterMain {
  lazy val sentencer = new BreezeSentencer
}

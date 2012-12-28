package edu.washington.cs.knowitall
package tool
package sentence

import breeze.text.segment.JavaSentenceSegmenter
import edu.washington.cs.knowitall.tool.segmenter.Segmenter
import edu.washington.cs.knowitall.tool.segmenter.SegmenterMain
import edu.washington.cs.knowitall.tool.tokenize.Tokenizer
import edu.washington.cs.knowitall.tool.segmenter.Segment
import edu.washington.cs.knowitall.tool.tokenize.Token

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

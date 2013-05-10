package edu.knowitall
package tool
package typer

import java.net.URL
import edu.stanford.nlp.ie.AbstractSequenceClassifier
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.util.Triple
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.Resource.using
import edu.knowitall.tool.tokenize._
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

class StanfordNer(private val classifier: AbstractSequenceClassifier[_]) extends Typer[Token]("Stanford", "Stanford") {
  def apply(seq: Seq[Token]) = {
    val text = Tokenizer.originalText(seq)
    import scala.collection.JavaConverters._
    val response = classifier.classifyToCharacterOffsets(text).asScala

    var tags = List.empty[Type]
    for (triple <- response) {
      val nerInterval = Interval.open(triple.second, triple.third)
      val nerType = triple.first

      // find actual token offsets from NER offsets
      val start = seq.iterator.zipWithIndex.find(_._1.offsets.start == nerInterval.start).map(_._2)
      val end = seq.iterator.zipWithIndex.find(_._1.offsets.end == nerInterval.end).map(_._2)

      for (s <- start; e <- end) {
        val typ = new Type(this.name + nerType, "Stanford", Interval.closed(s, e), text.substring(nerInterval.start, nerInterval.end))
        tags ::= typ
      }
    }

    tags
  }
}

object StanfordNer {
  final val defaultModelUrl = StanfordNer.getClass().getResource("/edu/stanford/nlp/models/ner/english.all.3class.distsim.crf.ser.gz")
  require(defaultModelUrl != null, "resource could not be found")

 def fromModelUrl(url: URL) = {
    using (url.openStream()) { stream =>
      new StanfordNer(CRFClassifier.getClassifier(new BufferedInputStream(new GZIPInputStream(stream))))
    }
  }

  def withDefaultModel = {
    fromModelUrl(defaultModelUrl)
  }
}

object StanfordNerMain extends LineProcessor("stanford-typer") {
  val tokenizer = PennTokenizer
  val ner = StanfordNer.withDefaultModel
  def process(line: String) = {
    ner(tokenizer(line)).mkString("\n")
  }
}

package edu.washington.cs.knowitall
package tool
package parse

class StanfordNer(private val classifier: AbstractSequenceClassifier[_])  extends Typer[Token] {
  val DEFAULT_MODEL = "edu/stanford/nlp/models/ner/all.3class.distsim.crf.ser.gz"

  override def apply(seq: Seq[Token]) = {
    val response = classifier.classifyToCharacterOffsets(sentence.originalText)

    var tags = List.empty[Type]
    for (triple <- response) {
      val nerInterval = Interval.open(triple.second, triple.third)
      val nerType = triple.first

      // find actual token offsets from NER offsets
      val start = seq.find(_.offset.start == nerInterval.start)
      val end = seq.find(_.offset.end == nerInterval.end)

      for (s <- start; e <- end) {
        val typ = new Type(this.descriptor + nerType, "Stanford", Interval.open(s, e), seq.slice(s, e))
        tags ::= new Type(this.descriptor + nerType, "Stanford", range)
      }
    }

    tags
  }
}

object StanfordNer {
  def withDefaultModel = {
    new StanfordNer(this.classifier = CRFClassifier.getClassifierNoExceptions(model))
  }
}

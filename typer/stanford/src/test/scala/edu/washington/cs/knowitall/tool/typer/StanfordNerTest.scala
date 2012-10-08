package edu.washington.cs.knowitall
package tool
package typer

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.washington.cs.knowitall.tool.tokenize.Tokenizer
import edu.washington.cs.knowitall.collection.immutable.Interval

@RunWith(classOf[JUnitRunner])
object StanfordNerTest extends Specification {
  "stanford ner example sentence" in {
    val text = "This is an example sentence for the Stanford named entity recognizer ."
    val split = text.split(" ")
    val tokens = Tokenizer.computeOffsets(split, text)
    
    val ner = StanfordNer.withDefaultModel
    val types = ner(tokens)
    
    types.size == 1
    types.head.interval == Interval.open(36, 44)
  }
}
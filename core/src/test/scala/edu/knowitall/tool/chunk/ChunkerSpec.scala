package edu.knowitall
package tool
package chunk

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import edu.knowitall.tool.tokenize._
import edu.knowitall.tool.postag._

@RunWith(classOf[JUnitRunner])
object ChunkerSpecTest extends Specification {
  "Chunker intervals are determined correctly." in {
    val strings = "John very quickly ran away from the deep blue reflection in the mirror .".split(" ")
    val postags = "NNP RB RB VBD RB IN DT JJ JJ NN IN DT NN .".split(" ")
    val chunks = "B-NP B-ADVP B-ADVP B-VP B-ADVP B-PP B-NP I-NP I-NP I-NP B-PP B-NP I-NP O".split(" ")

    val text = "John very quickly ran away from the deep blue reflection in the mirror."
    val tokens = Tokenizer.computeOffsets(strings, text)
    val chunkedTokens = (tokens zip postags zip chunks).map { case ((token, postag), chunk) =>
      new ChunkedToken(new PostaggedToken(token, postag), chunk)
    }

    Chunker.intervals(chunkedTokens).map(_.toString) must
      haveTheSameElementsAs(List("(NP,{0})", "(ADVP,{1})", "(ADVP,{2})", "(VP,{3})", "(ADVP,{4})", "(PP,{5})", "(NP,[6, 10))", "(PP,{10})", "(NP,[11, 13))", "(O,{13})"))
  }
}

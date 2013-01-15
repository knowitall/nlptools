package edu.washington.cs.knowitall
package tool
package coref

import common.ling.Word

import scala.collection.JavaConversions._

import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations._
import edu.stanford.nlp.dcoref._
import edu.stanford.nlp.util.CoreMap

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel

class StanfordCoreferenceResolver extends CoreferenceResolver {
  lazy val corenlp = {
    val props = new java.util.Properties();
    props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref");
    new StanfordCoreNLP(props);
  }

  // create a lookup
  override def clusters(text: String): Map[String, List[Mention]] = {
    val document = new Annotation(text);
    // run the Stanford pipeline
    corenlp.annotate(document);

    // an array of arrays, where the first dimension is sentences
    // and the second is tokens
    val tokens: Array[Array[CoreLabel]] = document.get[
      java.util.List[CoreMap],
      SentencesAnnotation
    ] (classOf[SentencesAnnotation]).map {sentence =>
      sentence.get[
          java.util.List[CoreLabel],
          TokensAnnotation
      ](classOf[TokensAnnotation]).toList.toArray
    }.toArray

    // stanford is doing some WEIRD stuff, look at the JavaDoc for get
    // somehow Java handles this without having to specify the types.
    val coremaps = document.get[
      java.util.Map[java.lang.Integer, CorefChain],
      CorefChainAnnotation
    ] (classOf[CorefChainAnnotation])

    (for ((k, chain) <- coremaps) yield {
      val representitive = chain.getRepresentativeMention
      val mentions = chain.getMentionsInTextualOrder

      (representitive.mentionSpan, mentions.map(m =>
        new Mention(m.mentionSpan, tokens(m.sentNum - 1)(m.startIndex - 1).beginPosition)
      ).toList)
    })(scala.collection.breakOut)
  }

  override def resolve(text: String, transform: (String,String)=>String): String = {
    val document = new Annotation(text);
    corenlp.annotate(document);

    // stanford is doing some WEIRD stuff, look at the JavaDoc for get
    // somehow Java handles this without having to specify the types.
    val coremaps = document.get[
      java.util.Map[java.lang.Integer, CorefChain],
      CorefChainAnnotation
    ] (classOf[CorefChainAnnotation])

    // build a map of spots to replace a mention with the
    // best mention (sentence, word) -> (mention, length)
    val replacements: Map[(Int, Int), (String, Int)] =
    (for { (k, chain) <- coremaps;
      representitive = chain.getRepresentativeMention
      mention <- chain.getMentionsInTextualOrder
      if chain.getMentionsInTextualOrder.size > 1
      if mention.mentionSpan != representitive.mentionSpan
    } yield {
      // switch to 0-indexing
      ((mention.sentNum-1, mention.startIndex-1),
      (representitive.mentionSpan, mention.endIndex - mention.startIndex))
    })(scala.collection.breakOut)

    val sentences = document.get[
      java.util.List[CoreMap],
      SentencesAnnotation
    ] (classOf[SentencesAnnotation]);

    val resolved = new StringBuilder((1.5 * text.length).toInt)

    // iterate over sentences
    for ((sentence, sentenceIndex) <- sentences.view.zipWithIndex) {
      val labels = sentence.get[
          java.util.List[CoreLabel],
          TokensAnnotation
        ](classOf[TokensAnnotation])

      // iterate over words of this sentence
      val iterator = labels.view.zipWithIndex.iterator()
      while (iterator.hasNext) {
        val (label, wordIndex) = iterator.next
        if (replacements.containsKey((sentenceIndex, wordIndex))) {
          val (string, length) = replacements((sentenceIndex, wordIndex))

          // skip over the other tokens of this mention
          val replaced = new StringBuffer((label.originalText.length + 1) * (length + 1))
          replaced.append(label.originalText)
          replaced.append(label.after)
          for (i <- 1 until length) {
            val (skip, _) = iterator.next()

            replaced.append(skip.originalText)
            replaced.append(skip.after)
          }

          val replacement = transform(replaced.toString, if (wordIndex == 0) Word.capitalize(string) else string)
          resolved.append(replacement)
        }
        else {
          resolved.append(label.originalText)
        }

        resolved.append(label.after)
      }
    }

    resolved.toString
  }
}

object StanfordCoreferenceResolver {
  def main(args: Array[String]) {
    def quote(s: String) = "\""+s+"\""
    val text = io.Source.stdin.getLines.mkString("\n")
    val resolver = new StanfordCoreferenceResolver()
    val mentions = resolver.mentions(text).toList
      .filter(_._2.size > 1).sortBy(- _._2.size)
    println(mentions.map{case (k, v) =>
      k+" -> "+v.mkString(", ")
    }.mkString("\n"))
    println(resolver.resolve(text, (original,replacement)=>original+"["+replacement+"]"))
  }
}

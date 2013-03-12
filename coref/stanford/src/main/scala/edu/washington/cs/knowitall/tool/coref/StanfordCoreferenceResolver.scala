package edu.knowitall
package tool
package coref

import java.io.File
import java.io.PrintWriter

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.mapAsJavaMap
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.JavaConversions.seqAsJavaList

import edu.knowitall.common.ling.Word
import edu.stanford.nlp.dcoref.CorefChain
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.util.CoreMap
import edu.knowitall.common.Resource.using

class StanfordCoreferenceResolver extends CoreferenceResolver {
  lazy val corenlp = {
    val props = new java.util.Properties();
    props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref");
    new StanfordCoreNLP(props);
  }

  // create a lookup
  override def clusters(text: String): Map[Mention, List[Mention]] = {
    val document = new Annotation(text);
    // run the Stanford pipeline
    corenlp.annotate(document);

    // an array of arrays, where the first dimension is sentences
    // and the second is tokens
    val tokens: Array[Array[CoreLabel]] = document.get[java.util.List[CoreMap], SentencesAnnotation](classOf[SentencesAnnotation]).map { sentence =>
      sentence.get[java.util.List[CoreLabel], TokensAnnotation](classOf[TokensAnnotation]).toList.toArray
    }.toArray

    // stanford is doing some WEIRD stuff, look at the JavaDoc for get
    // somehow Java handles this without having to specify the types.
    val coremaps = document.get[java.util.Map[java.lang.Integer, CorefChain], CorefChainAnnotation](classOf[CorefChainAnnotation])

    (for ((k, chain) <- coremaps) yield {
      val representitive = chain.getRepresentativeMention
      val mentions = chain.getMentionsInTextualOrder

      (new Mention(representitive.mentionSpan, tokens(representitive.sentNum - 1)(representitive.startIndex - 1).beginPosition), mentions.map(m =>
        new Mention(m.mentionSpan, tokens(m.sentNum - 1)(m.startIndex - 1).beginPosition)).toList)
    })(scala.collection.breakOut)
  }

  def resolve(text: String, transform: (String, String) => String): String = {
    val substitutions = this.substitutions(text).map { case Substitution(from, to) =>
      Substitution(from, to.copy(text = from.text + "[" + to.text + "]"))
    }

    CoreferenceResolver.substitute(text, substitutions)
  }
}

object StanfordCoreferenceResolverMain {
  case class Config(
    val inputFile: Option[File] = None,
    val outputFile: Option[File] = None,
    val bratOutput: Boolean = false) {

    def writer = outputFile match {
      case Some(file) => new PrintWriter(file)
      case None => new PrintWriter(System.out)
    }

    def source = inputFile match {
      case Some(file) => io.Source.fromFile(file, "UTF-8")
      case None => io.Source.stdin
    }
  }

  def main(args: Array[String]) {
    val parser = new scopt.immutable.OptionParser[Config]("coref") {
      def options = Seq(
        flag("brat", "brat output") { (c: Config) => c.copy(bratOutput = true) },
        opt("o", "output", "output file") { (path: String, c: Config) => c.copy(outputFile = Some(new File(path))) },
        opt("i", "input", "input file") { (path: String, c: Config) => c.copy(inputFile = Some(new File(path))) })
    }

    parser.parse(args, new Config()) match {
      case Some(config) => run(config)
      case None =>
    }
  }

  def run(config: Config) {
    def quote(s: String) = "\"" + s + "\""
    val text =
      using(config.source) { source =>
        source.getLines.mkString("\n")
      }
    val resolver = new StanfordCoreferenceResolver()

    val substitutions = resolver.substitutions(text).filter(_.best.text.size > 1).filter(sub => sub.mention != sub.best)
    using(config.writer) { writer =>
      if (config.bratOutput) {
        val mentions = substitutions.flatMap { case Substitution(actual, mention) => List(actual, mention) }

        val mentionMap = (mentions.zipWithIndex map { case (mention, index) => mention -> ("T" + index) }).toMap

        val entities = mentionMap.map { case (mention, name) => List(name, "Mention " + mention.charInterval.start + " " + mention.charInterval.end, mention.text) }
        val relations = substitutions.zipWithIndex map { case (Substitution(actual, mention), index) => List("R" + index, "Mention-of Arg1:" + mentionMap(actual) + " Arg2:" + mentionMap(mention), "") }

        entities.map(_ mkString "\t") foreach writer.println
        relations.map(_ mkString "\t") foreach writer.println
      } else {
        writer.println(substitutions.map {
          case Substitution(mention, best) =>
            mention + " -> " + best.text
        }.mkString("\n"))
        writer.println(resolver.resolve(text, (original, replacement) => original + "[" + replacement + "]"))
      }
    }
  }
}

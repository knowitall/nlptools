package edu.knowitall
package tool
package parse

import java.io.File
import java.net.URL
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.JavaConversions.asScalaSet
import org.maltparser.MaltParserService
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.parse.graph.Dependency
import graph.DependencyGraph
import graph.DependencyNode
import postag.OpenNlpPostagger
import postag.Postagger
import postag.PostaggedToken
import tokenize.Token
import stem.MorphaStemmer
import scala.collection.immutable.SortedSet

/** MaltParser is much faster than the StanfordParser but has a lower F-score.
  * It includes wrapper code so that it can still use the Stanford postprocessing.
  */
object MaltParserMain extends DependencyParserMain {
  var model = new File("engmalt.linear-1.7.mco")

  lazy val dependencyParser = new MaltParser(model);
}

class MaltParser(modelUrl: URL = new File("engmalt.linear-1.7.mco").toURI.toURL, tagger: Postagger = new OpenNlpPostagger, logFile: Option[File] = None) extends DependencyParser {
  val parser = initializeMaltParserService()
  val stemmer = MorphaStemmer

  def this(modelFile: File) = this(modelUrl = modelFile.toURI.toURL)

  private def initializeMaltParserService() = {
    // hack to make malt parser work with a different manifest
    import java.lang.reflect._
    val field = classOf[org.maltparser.core.helper.SystemInfo].getDeclaredField("version")
    field.setAccessible(true)
    field.set(null, "1.7")

    val command =
      "-u " + modelUrl +
      " -m parse" +
      // turn logging off if no log file is specified
      (logFile match {
        case Some(file) => " -lfi " + file.getPath
        case None => " -cl off"
      })

    System.err.println("Initializing malt: " + command);
    val service = new MaltParserService()

    try {
      service.initializeParserModel(command);
    }
    catch {
      case e: org.maltparser.core.config.ConfigurationException =>
        println("\n" + 
        "There was an error configurating MaltParser.\n" +
        "This is most likely because the model file '" + modelUrl + "' was not found.\n" +
        "Please download the MaltParser model file from http://www.maltparser.org.\n")

        throw e
    }

    service
  }

  private def clean(sentence: String) = {
    sentence.trim.
      // replace unicode double quotes
      replaceAll("[\u201c\u201d\u201e\u201f\u275d\u275e]", "\"").
      // replace unicode single quotes
      replaceAll("[\u2018\u2019\u201a\u201b\u275b\u275c]", "'")
  }

  override def dependencies(sentence: String): Iterable[Dependency] = {
    val trimmed = clean(sentence)
    if (trimmed.isEmpty) Iterable.empty
    else {
      val tokens = tagger.postag(trimmed)
      dependenciesPostagged(tokens)
    }
  }
    
  private def dependenciesTokenized(tokens: Seq[Token]): Iterable[Dependency] = {
    val postaggedTokens = tagger.postagTokens(tokens)
    dependenciesPostagged(postaggedTokens)
  }
  
  private def dependenciesPostagged(tokens: Seq[PostaggedToken]): Iterable[Dependency] = {
 
    val nodes = tokens.iterator.zipWithIndex.map { case (t, i) =>
      new DependencyNode(t, Interval.singleton(i))
    }.toIndexedSeq

    val lemmatized = nodes.map(stemmer.stemToken)

    val maltTokens: Array[String] = lemmatized.iterator.zipWithIndex.map { case (ltok, i) =>
      Iterable(i+1,
          ltok.token.string,
          ltok.lemma,
          ltok.token.postag,
          ltok.token.postag,
          "-").mkString("\t")
    }.toArray[String]
    val structure = parser.parse(maltTokens)

    val tables = structure.getSymbolTables

    val deps: SortedSet[Dependency] = structure.getEdges.flatMap { edge =>
      if (edge.getSource.getIndex == 0 || edge.getTarget.getIndex == 0) {
        // skip the root
        None
      }
      else {
        val source = nodes(edge.getSource.getIndex - 1)
        val dest = nodes(edge.getTarget.getIndex - 1)

        val types = edge.getLabelTypes
        val labels = types.map(edge.getLabelSymbol)
        val label = labels.head

        Some(new Dependency(source, dest, label))
      }
    }(scala.collection.breakOut)

    deps
  }

  override def dependencyGraph(sentence: String): DependencyGraph = {
    val deps = dependencies(sentence)
    val nodes: Set[DependencyNode] = deps.flatMap(dep => Set(dep.source, dep.dest))(scala.collection.breakOut)
    new DependencyGraph(nodes, deps)
  }

  def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    val deps = dependenciesPostagged(tokens)
    val nodes: Set[DependencyNode] = deps.flatMap(dep => Set(dep.source, dep.dest))(scala.collection.breakOut)
    new DependencyGraph(nodes, deps)
  }

  def dependencyGraphTokenized(tokens: Seq[Token]): DependencyGraph = {
    val deps = dependenciesTokenized(tokens)
    val nodes: Set[DependencyNode] = deps.flatMap(dep => Set(dep.source, dep.dest))(scala.collection.breakOut)
    new DependencyGraph(nodes, deps)
  }
}

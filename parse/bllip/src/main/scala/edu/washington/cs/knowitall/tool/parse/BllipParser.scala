package edu.washington.cs.knowitall
package tool
package parse

import scala.collection.JavaConverters.seqAsJavaListConverter
import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.parser.charniak.CharniakParser
import edu.washington.cs.knowitall.tool.parse.BaseStanfordParser.CollapseType
import edu.washington.cs.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.washington.cs.knowitall.tool.tokenize.Tokenizer
import graph.Dependency
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import java.lang.ProcessBuilder
import java.io.PrintWriter

class BllipParser(val tokenizer: Tokenizer) extends BaseStanfordParser with ConstituencyParser {
  def this() = this(new OpenNlpTokenizer())

  val blipp = new CharniakParser("bllip-parser/first-stage/PARSE/parseIt", "bllip-parser/first-stage/DATA/EN/")

  object BllipProcess {
    import sys.process._

    private var result: String = _
    private val bllip = "bllip-parser/first-stage/PARSE/parseIt" run BasicIO(false, (line: String) => result = line, None).writeInput
  }

  private def parseHelper(string: String) = {
    import scala.collection.JavaConverters._

    val tokens = tokenizer(string)
    val words = tokens.map(token => new Word(token.string, token.interval.start, token.interval.end)).asJava

    blipp.getBestParse(words)
  }

  override def dependencyGraph(string: String, collapse: CollapseType): DependencyGraph = {
    import scala.collection.JavaConverters._

    val tokens = tokenizer(string)
    val words = tokens.map(token => new Word(token.string, token.interval.start, token.interval.end)).asJava

    val tree = blipp.getBestParse(words)

    val (nodes, deps) = StanfordParser.dependencyHelper(tree, collapse)

    def withOffset(node: DependencyNode) =
      new DependencyNode(node.string, node.postag, node.indices, tokens(node.indices.head).offset)

    val nodesWithOffsets = nodes map (node => withOffset(node))
    val depsWithOffsets = deps.map(_.mapNodes(node => withOffset(node)))
    new DependencyGraph(string, nodesWithOffsets.toList.sortBy(_.indices), depsWithOffsets)
  }

  def parse(string: String) = {
    StanfordParser.convertTree(this.parseHelper(string))
  }
}

object BlippDependencyParserMain extends DependencyParserMain {
  lazy val parser = new BllipParser
}

object BlippConstituencyParserMain
  extends ConstituencyParserMain {
  lazy val parser = new BllipParser
}

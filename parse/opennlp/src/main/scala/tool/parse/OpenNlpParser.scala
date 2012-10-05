package edu.washington.cs.knowitall
package tool
package parse

import opennlp.tools.cmdline.parser.ParserTool
import opennlp.tools.parser._

class OpenNlpParser(val model: ParserModel) extends ConstituencyParser {
  def this(modelName: String = OpenNlpParser.defaultModelName) =
    this(new ParserModel(
        classOf[OpenNlpParser].getResourceAsStream(modelName)))

  val parser = ParserFactory.create(model)

  def parse(sentence: String) = {
    var index = 0
    def convertTree(tree: opennlp.tools.parser.Parse): ParseTree = {
      val curindex = index
      index += 1
      val value = if (tree.getType == "TK") sentence.substring(tree.getSpan.getStart, tree.getSpan.getEnd) else tree.getType
      val children = tree.getChildren.map(child => convertTree(child))
      if (tree.getChildCount > 0 && !tree.isPosTag)
        new ParseTreePhrase(value, curindex, children)
      else if (tree.isPosTag)
        new ParseTreePostag(value, curindex, children)
      else
        new ParseTreeToken(value, curindex, children)
    }

    val parse = ParserTool.parseLine(sentence, parser, 1)(0)
    convertTree(parse)
  }
}

object OpenNlpParser {
  val defaultModelName = "/en-parser-chunking.bin"
}

object OpenNlpParserMain extends ConstituencyParserMain {
  lazy val parser = new OpenNlpParser() 
}

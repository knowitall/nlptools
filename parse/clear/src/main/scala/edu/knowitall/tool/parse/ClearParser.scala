package edu.knowitall
package tool
package parse

import com.clearnlp.nlp.NLPGetter
import com.clearnlp.nlp.NLPLib
import com.clearnlp.dependency.DEPNode
import com.clearnlp.dependency.DEPTree

import edu.knowitall.common.Resource.using
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.graph.DependencyNode
import edu.knowitall.tool.postag.ClearPostagger
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.postag.Postagger
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.tokenize.Tokenizer

import graph.Dependency

import java.io.PrintWriter
import java.lang.ProcessBuilder
import java.util.zip.ZipInputStream

import scala.collection.JavaConverters._

class ClearParser(val postagger: Postagger = new ClearPostagger()) extends DependencyParser {
  val clearMorpha = NLPGetter.getComponent("general-en", "en", NLPLib.MODE_MORPH)

  val clearDepParser = NLPGetter.getComponent("general-en", "en", NLPLib.MODE_DEP)

  def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    val tree = new DEPTree()
    tokens.zipWithIndex.foreach { case (token, i) =>
      val node = new DEPNode(i + 1, token.string)
      node.pos = token.postag
      tree.add(node)
    }

    clearMorpha.process(tree)
    clearDepParser.process(tree)

    ClearParser.graphFromTree(tree, tokens)
  }
}

object ClearParser {
  def graphFromTree(tree: DEPTree, tokens: Seq[Token]): DependencyGraph = {
    val nodeMap = (for ((node, i) <- tree.iterator.asScala.zipWithIndex) yield {
      if (i == 0) node.id -> new DependencyNode(-1, node.form)
      else node.id -> new DependencyNode(i, node.form)
    }).toMap
    
    val deps = for {
      sourceNode <- tree.iterator.asScala.toList
      if sourceNode.hasHead
      if sourceNode.id != 0
      label = sourceNode.getLabel
      destNode = sourceNode.getHead
      if destNode.id != 0
    } yield {
      new Dependency(nodeMap(destNode.id), nodeMap(sourceNode.id), label)
    }

    new DependencyGraph(nodeMap.values.toSet filterNot (_.id == -1), deps.toSet)
  }
}

object ClearDependencyParserMain extends DependencyParserMain {
  lazy val dependencyParser = new ClearParser()
}

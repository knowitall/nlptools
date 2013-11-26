package edu.knowitall.tool.srl

import java.util.zip.ZipInputStream

import scala.Option.option2Iterable
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.io.Source

import com.clearnlp.nlp.NLPGetter
import com.clearnlp.nlp.NLPLib
import com.clearnlp.dependency.DEPNode
import com.clearnlp.dependency.DEPTree

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.Resource.using
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.parse.graph.DependencyGraph

class ClearSrl extends Srl {
  private val modelType = "general-en"
  private val language = "en"

  private val clearMorpha = NLPGetter.getComponent(modelType, language, NLPLib.MODE_MORPH)
  private val clearRoles = NLPGetter.getComponent(modelType, language, NLPLib.MODE_ROLE)
  private val clearPred = NLPGetter.getComponent(modelType, language, NLPLib.MODE_PRED)
  private val clearSrl = NLPGetter.getComponent(modelType, language, NLPLib.MODE_SRL)

  def apply(graph: DependencyGraph): Seq[Frame] = {
    val tree = new DEPTree()

    graph.nodes.zipWithIndex.foreach {
      case (token, i) =>
        val node = new DEPNode(i + 1, token.string)
        // node.pos = token.postag
        tree.add(node)
    }

    for (edge <- graph.dependencies) {
      val source = tree.get(edge.source.id + 1)
      val dest = tree.get(edge.dest.id + 1)
      dest.setHead(source, edge.label)
    }

    // link all nodes other than the root (hence the drop 1)
    // to the root node.
    for (node <- tree.iterator.asScala.drop(1)) {
      if (node.getHead == null) {
        node.setHead(tree.get(0), "root")
      }
    }

    clearMorpha.process(tree)
    clearPred.process(tree)
    clearRoles.process(tree)
    clearSrl.process(tree)

    val treeNodes = tree.asScala.toSeq
    val relations = treeNodes.flatMap { node =>
      val index = node.id - 1
      Option(node.getFeat("pb")).map(index -> Relation.fromString(graph.nodes.find(_.id == index).get, _))
    }

    val arguments = treeNodes.flatMap { node =>
      val index = node.id - 1
      node.getSHeads().asScala.map { head =>
        (head.getNode.id - 1) -> Argument(graph.nodes.find(_.id == index).get, Roles(head.getLabel))
      }
    }

    relations.map {
      case (index, rel) =>
        val args = arguments.filter(_._1 == index)
        new Frame(rel, args.map(_._2))
    }
  }
}

object ClearSrlMain extends SrlMain {
  override val srl = new ClearSrl()
}

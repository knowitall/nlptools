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
import edu.knowitall.tool.postag.PostaggedToken

class ClearSrl extends Srl {
  private val modelType = "general-en"
  private val language = "en"

  private val clearMorpha = NLPGetter.getComponent(modelType, language, NLPLib.MODE_MORPH)
  private val clearRoles = NLPGetter.getComponent(modelType, language, NLPLib.MODE_ROLE)
  private val clearPred = NLPGetter.getComponent(modelType, language, NLPLib.MODE_PRED)
  private val clearSrl = NLPGetter.getComponent(modelType, language, NLPLib.MODE_SRL)

  def apply(tokens: Seq[PostaggedToken], graph: DependencyGraph): Seq[Frame] = {
    // A new tree comes with a root node.
    val tree = new DEPTree()

    graph.nodes.foreach { token =>
      val node = new DEPNode(token.id + 1, token.string)
      node.pos = tokens(token.id).postag
      tree.add(node)
    }

    for (edge <- graph.dependencies) {
      val source = tree.get(edge.source.id + 1)
      require(source != null, "No dest node found in DEPTree: " + (edge.source.id + 1))
      val dest = tree.get(edge.dest.id + 1)
      require(dest != null, "No source node found in DEPTree: " + (edge.dest.id + 1))
      dest.setHead(source, edge.label)
    }

    // link all nodes other than the root (hence the drop 1)
    // to the root node if they have no parent.
    val root = tree.get(0)
    for (node <- tree.iterator.asScala filter (_.id != 0)) {
      if (node.getHead == null) {
        node.setHead(root, "root")
      }
    }

    clearMorpha.process(tree)
    clearPred.process(tree)
    clearRoles.process(tree)
    clearSrl.process(tree)

    val treeNodes = tree.asScala.toSeq
    val relations = treeNodes.flatMap { node =>
      Option(node.getFeat("pb")).map(node.id -> Relation.fromString(graph.nodes.find(_.id + 1 == node.id).get, _))
    }

    val arguments = treeNodes.flatMap { node =>
      node.getSHeads().asScala.map { head =>
        (head.getNode.id) -> Argument(graph.nodes.find(_.id + 1 == node.id).get, Roles(head.getLabel))
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

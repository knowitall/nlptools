package edu.washington.cs.knowitall.tool.srl

import java.util.zip.ZipInputStream

import scala.Option.option2Iterable
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.io.Source

import com.googlecode.clearnlp.component.morph.CEnglishMPAnalyzer
import com.googlecode.clearnlp.component.srl.CPredIdentifier
import com.googlecode.clearnlp.component.srl.CRolesetClassifier
import com.googlecode.clearnlp.component.srl.CSRLabeler
import com.googlecode.clearnlp.dependency.DEPNode
import com.googlecode.clearnlp.dependency.DEPTree

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.Resource.using
import edu.washington.cs.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

class ClearSrl {
  val clearMorpha = using(this.getClass.getResource("/edu/washington/cs/knowitall/tool/tokenize/dictionary-1.2.0.zip").openStream()) { input =>
    new CEnglishMPAnalyzer(new ZipInputStream(input))
  }
  val clearRoles = using(this.getClass.getResource("/knowitall/models/clear/ontonotes-en-role-1.3.0.jar").openStream()) { input =>
    new CRolesetClassifier(new ZipInputStream(input))
  }
  val clearPred = using(this.getClass.getResource("/knowitall/models/clear/ontonotes-en-pred-1.3.0.jar").openStream()) { input =>
    new CPredIdentifier(new ZipInputStream(input))
  }
  val clearSrl = using(this.getClass.getResource("/knowitall/models/clear/ontonotes-en-srl-1.3.0.jar").openStream()) { input =>
    new CSRLabeler(new ZipInputStream(input))
  }

  def apply(graph: DependencyGraph) = {
    val tree = new DEPTree()

    graph.nodes.zipWithIndex.foreach {
      case (token, i) =>
        val node = new DEPNode(i + 1, token.string)
        node.pos = token.postag
        tree.add(node)
    }

    for (edge <- graph.dependencies) {
      val source = tree.get(edge.source.indices.head + 1)
      val dest = tree.get(edge.dest.indices.head + 1)
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
      Option(node.getFeat("pb")).map(index -> Relation.fromString(graph.nodes.find(_.indices == Interval.singleton(index)).get, _))
    }

    val arguments = treeNodes.flatMap { node =>
      val index = node.id - 1
      node.getSHeads().asScala.map { head =>
        (head.getNode.id - 1) -> Argument(graph.nodes.find(_.indices == Interval.singleton(index)).get, Roles(head.getLabel))
      }
    }

    relations.map {
      case (index, rel) =>
        val args = arguments.filter(_._1 == index)
        new Frame(rel, args.map(_._2))
    }
  }
}

object ClearSrlMain extends App {
  val parser = new ClearParser()
  val srl = new ClearSrl()
  val source = Source.stdin
  for (line <- source.getLines) {
    val graph = parser.dependencyGraph(line)
    println(graph.serialize)
    val frames = srl.apply(graph)
    frames foreach println
    frames map (_.serialize) foreach println
    val hierarchy = FrameHierarchy.fromFrames(graph, frames.toIndexedSeq)
    hierarchy foreach println
  }
}

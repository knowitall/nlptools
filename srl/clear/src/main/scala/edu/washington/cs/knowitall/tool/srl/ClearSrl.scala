package edu.washington.cs.knowitall
package tool
package srl

import java.util.zip.ZipInputStream
import scala.collection.JavaConverters.asScalaIteratorConverter
import com.googlecode.clearnlp.component.dep.CDEPPassParser
import com.googlecode.clearnlp.component.morph.CEnglishMPAnalyzer
import com.googlecode.clearnlp.dependency.DEPNode
import scala.collection.JavaConverters._
import com.googlecode.clearnlp.dependency.DEPTree
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.parse.graph.Dependency
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.postag.Postagger
import edu.washington.cs.knowitall.tool.postag.ClearPostagger
import edu.washington.cs.knowitall.tool.parse.ClearParser
import com.googlecode.clearnlp.component.srl.CRolesetClassifier
import com.googlecode.clearnlp.component.srl.CSRLabeler
import com.googlecode.clearnlp.component.pos.CPOSTagger
import edu.washington.cs.knowitall.tool.tokenize.Tokenizer
import edu.washington.cs.knowitall.tool.tokenize.ClearTokenizer
import com.googlecode.clearnlp.component.srl.CPredIdentifier
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import edu.washington.cs.knowitall.tool.parse.DependencyParser
import scala.io.Source

class ClearSrl {
  /*
  val clearPostag = using(this.getClass.getResource("/edu/washington/cs/knowitall/tool/postag/ontonotes-en-pos-1.3.0.jar").openStream()) { input =>
    new CPOSTagger(new ZipInputStream(input));
  }
  */
  val clearMorpha = using(this.getClass.getResource("/edu/washington/cs/knowitall/tool/tokenize/dictionary-1.2.0.zip").openStream()) { input =>
    new CEnglishMPAnalyzer(new ZipInputStream(input))
  }
  /*
  val clearDepParser = using(this.getClass.getResource("/edu/washington/cs/knowitall/tool/parse/ontonotes-en-dep-1.3.0.jar").openStream()) { input =>
    new CDEPPassParser(new ZipInputStream(input))
  }
  */
  val clearRoles = using(this.getClass.getResource("ontonotes-en-role-1.3.0.jar").openStream()) { input =>
    new CRolesetClassifier(new ZipInputStream(input))
  }
  val clearPred = using(this.getClass.getResource("ontonotes-en-pred-1.3.0.jar").openStream()) { input =>
    new CPredIdentifier(new ZipInputStream(input))
  }
  val clearSrl = using(this.getClass.getResource("ontonotes-en-srl-1.3.0.jar").openStream()) { input =>
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
  val text = "Michael Schmitz wants to move to france and buy a yacht."
  val someGraph = "nn(Schmitz_NNP_1_8, Michael_NNP_0_0); nsubj(wants_VBZ_2_16, Schmitz_NNP_1_8); xcomp(wants_VBZ_2_16, move_VB_4_25); punct(wants_VBZ_2_16, ._._11_55); aux(move_VB_4_25, to_TO_3_22); prep(move_VB_4_25, to_IN_5_30); cc(move_VB_4_25, and_CC_7_40); conj(move_VB_4_25, buy_VB_8_44); pobj(to_IN_5_30, france_NN_6_33); dobj(buy_VB_8_44, yacht_NN_10_50); det(yacht_NN_10_50, a_DT_9_48)"
  // val graph = parser.dependencyGraph(text)
  val source = Source.stdin
  for (line <- source.getLines) {
    val graph = parser.dependencyGraph(line)
    println(graph.serialize)
    srl.apply(graph) foreach println
  }
}

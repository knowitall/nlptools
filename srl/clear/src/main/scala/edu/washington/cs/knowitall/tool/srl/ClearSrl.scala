package edu.washington.cs.knowitall
package tool
package srl

import java.util.zip.ZipInputStream
import scala.collection.JavaConverters.asScalaIteratorConverter
import com.googlecode.clearnlp.component.dep.CDEPPassParser
import com.googlecode.clearnlp.component.morph.CEnglishMPAnalyzer
import com.googlecode.clearnlp.dependency.DEPNode
import com.googlecode.clearnlp.dependency.DEPTree
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.tool.parse.graph.Dependency
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.postag.Postagger
import edu.washington.cs.knowitall.tool.postag.ClearPostagger
import com.googlecode.clearnlp.component.srl.CRolesetClassifier
import com.googlecode.clearnlp.component.srl.CSRLabeler
import com.googlecode.clearnlp.component.pos.CPOSTagger
import edu.washington.cs.knowitall.tool.tokenize.Tokenizer
import edu.washington.cs.knowitall.tool.tokenize.ClearTokenizer

class ClearSrl(tokenizer: Tokenizer = new ClearTokenizer()) {
  val clearPostag = using(this.getClass.getResource("/edu/washington/cs/knowitall/tool/postag/ontonotes-en-pos-1.3.0.jar").openStream()) { input =>
    new CPOSTagger(new ZipInputStream(input));
  }
  val clearMorpha = using(this.getClass.getResource("/edu/washington/cs/knowitall/tool/tokenize/dictionary-1.2.0.zip").openStream()) { input =>
    new CEnglishMPAnalyzer(new ZipInputStream(input))
  }
  val clearDepParser = using (this.getClass.getResource("ontonotes-en-dep-1.3.0.jar").openStream()) { input =>
    new CDEPPassParser(new ZipInputStream(input))
  }
  val clearRoles = using (this.getClass.getResource("ontonotes-en-role-1.3.0.jar").openStream()) { input =>
    new CRolesetClassifier(new ZipInputStream(input))
  }
  val clearPred = using (this.getClass.getResource("ontonotes-en-pred-1.3.0.jar").openStream()) { input =>
    new CSRLabeler(new ZipInputStream(input))
  }
  val clearSrl = using (this.getClass.getResource("ontonotes-en-srl-1.3.0.jar").openStream()) { input =>
    new CSRLabeler(new ZipInputStream(input))
  }

  def apply(string: String) = {
    val tokens = tokenizer(string)
    val tree = new DEPTree()
    tokenizer(string).zipWithIndex.foreach { case (token, i) =>
      tree.add(new DEPNode(i + 1, token.string))
    }

    println("postag")
    clearPostag.process(tree)
    println(tree.toStringPOS())
    println("morpha")
    clearMorpha.process(tree)
    println(tree.toStringMorph())
    println("dep")
    clearDepParser.process(tree)
    println(tree.toStringDEP())
    println("role")
    clearRoles.process(tree)
    println("pred")
    clearPred.process(tree)
    println("srl")
    clearSrl.process(tree)
    println(tree.toStringSRL())

    /*
    val nodeMap = (for ((node, i) <- tree.iterator.asScala.drop(1).zipWithIndex) yield {
      node -> new DependencyNode(node.form, node.pos, i, tokens(i).offset)
    }).toMap

    val deps = for {
      sourceNode <- tree.iterator.asScala.drop(1).toList
      if sourceNode.hasHead
      val label = sourceNode.getLabel
      if label != "root"
      val destNode = sourceNode.getHead
    } yield {
      new Dependency(nodeMap(destNode), nodeMap(sourceNode), label)
    }
    */

    ""
  }
}

object ClearSrlMain extends App {
  println("before")
  new ClearSrl().apply("Michael ran a marathon on Tuesday.").toString
  println("after")
}

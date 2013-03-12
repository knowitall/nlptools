package edu.knowitall
package tool
package postag

import java.util.zip.ZipInputStream
import scala.collection.JavaConverters.asScalaIteratorConverter
import com.googlecode.clearnlp.component.pos.CPOSTagger
import com.googlecode.clearnlp.dependency.DEPNode
import com.googlecode.clearnlp.dependency.DEPTree
import edu.knowitall.common.Resource.using
import edu.knowitall.tool.tokenize.Tokenizer
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.tokenize.Token

class ClearPostagger(override val tokenizer: Tokenizer = new ClearTokenizer) extends Postagger(tokenizer) {
  val clearPosUrl = this.getClass.getResource("/knowitall/models/clear/ontonotes-en-pos-1.3.0.jar")
  require(clearPosUrl != null, "clear pos model not found")
  val clearPosTagger = using (clearPosUrl.openStream()) { input =>
    new CPOSTagger(new ZipInputStream(input));
  }

  override def postagTokens(tokens: Seq[Token]) = {
    val tree = new DEPTree()
    tokens.zipWithIndex.foreach { case (token, i) =>
      tree.add(new DEPNode(i + 1, token.string))
    }

    clearPosTagger.process(tree)

    val postaggedTokens = for ((treeNode, token) <- (tree.iterator.asScala.drop(1).toSeq zip tokens)) yield {
      new PostaggedToken(token, treeNode.pos)
    }

    postaggedTokens
  }
}

object ClearPostaggerMain extends PostaggerMain {
  override val tagger = new ClearPostagger()
}

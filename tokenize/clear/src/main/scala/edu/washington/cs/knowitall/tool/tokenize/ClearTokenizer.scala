package edu.washington.cs.knowitall
package tool
package tokenize

import scala.collection.JavaConversions._
import edu.washington.cs.knowitall.common.Resource.using
import com.googlecode.clearnlp.engine.EngineGetter
import com.googlecode.clearnlp.tokenization.EnglishTokenizer
import java.util.zip.ZipInputStream
import java.net.URL

class ClearTokenizer(modelUrl: URL = ClearTokenizer.defaultModelUrl)
extends Tokenizer {
  require(modelUrl != null, "model url is null")

  val tokenizer =
    using(modelUrl.openStream()) { inputStream =>
      new EnglishTokenizer(new ZipInputStream(inputStream))
    }

  def tokenize(sentence: String): Seq[Token] = {
    val strings = tokenizer.getTokens(sentence)
    Tokenizer.computeOffsets(strings, sentence)
  }
}

object ClearTokenizer {
  val defaultModelUrl = classOf[ClearTokenizer].getResource("dictionary-1.2.0.zip")
}

object ClearTokenizerMain extends TokenizerMain {
  val tokenizer = new ClearTokenizer()
}

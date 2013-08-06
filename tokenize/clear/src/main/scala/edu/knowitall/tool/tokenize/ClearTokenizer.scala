package edu.knowitall
package tool
package tokenize

import scala.collection.JavaConversions._
import edu.knowitall.common.Resource.using
import com.googlecode.clearnlp.engine.EngineGetter
import com.googlecode.clearnlp.tokenization.EnglishTokenizer
import java.util.zip.ZipInputStream
import java.net.URL

class ClearTokenizer(modelUrl: URL = ClearTokenizer.defaultModelUrl)
extends Tokenizer {
  require(modelUrl != null, "tokenizer model url is null")

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
  val defaultModelUrl = {
    val url = "dictionary-1.2.0.zip"
    val resource = classOf[ClearTokenizer].getResource(url)
    require(resource != null, "Could not find tokenizer models: " + url)
    resource
  }
}

object ClearTokenizerMain extends TokenizerMain {
  val tokenizer = new ClearTokenizer()
}

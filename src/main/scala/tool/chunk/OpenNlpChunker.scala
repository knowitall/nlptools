package edu.washington.cs.knowitall
package tool
package chunk

import opennlp.tools.chunker._

class OpenNlpChunker(
  val model: ChunkerModel,
  postagger: postag.PosTagger) 
extends Chunker(postagger) {
  def this(modelName: String = "en-chunker.bin", 
    postagger: postag.PosTagger = new postag.OpenNlpPosTagger()) = 
    this(new ChunkerModel(OpenNlpChunker.loadModel(modelName)), postagger)

  val chunker = new ChunkerME(model)

  override def chunk(strings: Array[String], postags: Array[String]) = {
    val chunks = chunker.chunk(strings, postags)
    chunks
  }
}

object OpenNlpChunker extends ChunkerMain {
  lazy val chunker = new OpenNlpChunker("en-chunker.bin", new postag.OpenNlpPosTagger())

  private def loadModel(name: String) = {
    val resource = classOf[OpenNlpChunker].getClassLoader.getResourceAsStream(name)
    if (resource == null) throw new IllegalArgumentException("could not find resource: " + name)
    else resource
  }
}

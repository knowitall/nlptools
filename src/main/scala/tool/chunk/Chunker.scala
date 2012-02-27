package edu.washington.cs.knowitall
package tool
package chunk

import common.main.LineProcessor

abstract class Chunker(val postagger: postag.PosTagger) {
  /**
    * @returns chunk */
  def chunk(strings: Array[String], postags: Array[String]): Array[String]

  /**
    * @returns (strings, chunk) */
  def chunk(strings: Array[String]): IndexedSeq[(String,String)] = {
    val postags = postagger.postag(strings)
    postags zip chunk(strings, postags)
  }

  /**
    * @returns (strings, (postag, chunk)) */
  def chunk(sentence: String): IndexedSeq[(String,(String,String))] = {
    val unzipped = postagger.postag(sentence).unzip
    unzipped._1 zip (unzipped._2 zip chunk(unzipped._1.toArray, unzipped._2.toArray))
  }
}

abstract class ChunkerMain
extends LineProcessor {
  def chunker: Chunker
  override def process(line: String) = chunker.chunk(line).map{case (a,(b,c)) => a + "/" + b + "/" + c}.mkString(" ")

  override def init(args: Array[String]) {
    // for timing purposes
    chunker.chunk("I want to initialize the chunker.")
  }

  override def exit(ns: Long) {
    System.err.println(ns / 1000 / 1000 + "ms")
  }
}

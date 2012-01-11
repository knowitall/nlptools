package edu.washington.cs.knowitall
package tool
package parse

import edu.washington.cs.knowitall.common.Timing._

object ParserSpeedTest {
  def main(args: Array[String]) {
    val parsers = List(new StanfordParser(), new MaltParser())
    val sentences = List(
      "I mash and stir apples and bananas.",
      "Barack Obama is the current president of the United States.",
      "A collection of 15 million ReVerb extractions is available for academic use.",
      "The extractions are the result of running ReVerb on the ClueWeb09 dataset.",
      "With this sin of disobedience in him, Jonah still further flouts at God, by seeking to flee from Him.",
      "Annual spending for U.S. intelligence operations currently totals $ 47.5 billion , a figure that does not include expensive satellites that fall under the Pentagon 's budget .",
      "I mash and stir apples and bananas.",
      "Barack Obama is the current president of the United States.",
      "A collection of 15 million ReVerb extractions is available for academic use.",
      "The extractions are the result of running ReVerb on the ClueWeb09 dataset.",
      "With this sin of disobedience in him, Jonah still further flouts at God, by seeking to flee from Him.",
      "Annual spending for U.S. intelligence operations currently totals $ 47.5 billion , a figure that does not include expensive satellites that fall under the Pentagon 's budget .")

    val examples = if (args.length == 0) {
      println("Using built-in sentences.")
      sentences 
    }
    else {
      println("Using sentences from specified file.")
      io.Source.fromFile(args(0)).getLines.toList
    }

    println("dry run...")
    for (parser <- parsers) {
      println(parser.getClass.getSimpleName)
      println(speedTest(examples.map{ sentence => () => parser.dependencies(sentence) }).toString)
    }
    
    println("timing...")
    for (parser <- parsers) {
      println(parser.getClass.getSimpleName)
      println(speedTest(examples.map{ sentence => () => parser.dependencies(sentence) }).toString)
    }
  }
}

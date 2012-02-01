package edu.washington.cs.knowitall
package tool
package stem

import java.io.StringReader

import uk.ac.susx.informatics.Morpha
import common.main.LineProcessor

class MorphaStemmer extends Stemmer {
  def stem(word: String) = this.morpha(word)

  def morpha(text: String) = {
    val textParts = text.split(" ")

    val result = new StringBuilder
    try {
      for (i <- 0 until textParts.length) {
        val morpha = new Morpha(new StringReader(textParts(i)), false)
        result.append((if (result.length() == 0) "" else " ") + morpha.next())
      }
    }
    // yes, Morpha is cool enough to throw Errors
    // usually when the text contains underscores
    catch {
      case e: Error => text
      case e: java.io.IOException => text
    }

    result.toString
  }
}

object MorphaStemmer
extends StemmerMain {
  lazy val stemmer = new MorphaStemmer
  def instance = stemmer
}

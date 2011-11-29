package edu.washington.cs.knowitall
package tool
package tokenize

import common.main.LineProcessor

import scala.collection.JavaConversions._

import edu.stanford.nlp.ling._;
import edu.stanford.nlp.process._;

import java.util.regex._

class PennTokenizer extends Tokenizer {
  val replacements = List(
    // attempt to get correct directional quotes
    ("^\"", "`` "),
    //("""([ (\[{<])""", "$1 `` "),
    ("""\.\.\.""", " ... "),
    ("[,;:@#$%&]", " $0 "),
    ("""([^.]\)\([.])([])}>"']*)[ 	]*$""", "$1 $2$3 "),
    ("[?!]", " $0 "),
    ("""[](){}<>]""", " $0 "),
    ("--", " $0 "),
    ("$|^", " "),
    ("\"", " '' "),
    (""" ([^'])' """, " '$1 "),
    ("""'([sSmMdD]) """, " '$1 "),
    ("'(ll|re|ve|LL|RE|VE) ", " '$1 "),
    ("(n't|N'T) ", " $1 ")
  ).map { case (a, b) => 
    (Pattern.compile(a), b)
  }

  def tokenize(sentence: String) =
    replacements.foldRight(sentence) { case ((t, r), s) =>
      t.matcher(s).replaceAll(r)
    }.trim.split("\\s+")
}

object PennTokenizer extends LineProcessor {
  val tokenizer = new PennTokenizer()
  override def process(sentence: String) = 
    tokenizer.tokenize(sentence).mkString(" ")
}

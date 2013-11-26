package edu.knowitall
package tool
package parse

object StanfordConstituencyParserMain
  extends ConstituencyParserMain {
  lazy val constituencyParser = new StanfordParser();
}

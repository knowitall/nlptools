package edu.knowitall
package tool
package parse

object StanfordParserMain extends DependencyParserMain {
  lazy val dependencyParser = new StanfordParser
}


package edu.knowitall
package tool
package tokenize

import org.junit._
import org.junit.Assert._
import org.junit.runner.RunWith

import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object TokenSpecTest extends Specification {
  "tokens serialize and deserialize correctly" in {
    val token = Token("asdf", 0)
    Token.deserialize(token.serialize) == token
  }
}

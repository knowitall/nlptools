package edu.washington.cs.knowitall
package tool
package coref

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import edu.knowitall.collection.immutable.Interval

@RunWith(classOf[JUnitRunner])
object CoreferenceResolverTest extends Specification {
  "substitute method" in {
    val text = "one two three four five."
    val indexedText = text.zipWithIndex
    val substitutions = Seq(
      Interval.open(0, 3) -> "eleven",
      Interval.open(4, 7) -> "twelve",
      Interval.open(8, 13) -> "thirteen")

    CoreferenceResolver.substituteBase(text, substitutions) must_== "eleven twelve thirteen four five."
    CoreferenceResolver.substituteBase(text, substitutions.reverse) must_== "eleven twelve thirteen four five."

    CoreferenceResolver.substitute(indexedText, substitutions).map(_._1).mkString must_== "eleven twelve thirteen four five."
    CoreferenceResolver.substitute(indexedText, substitutions.reverse).map(_._1).mkString must_== "eleven twelve thirteen four five."
  }

  "substitute with indices" in {
    val coref = new StanfordCoreferenceResolver()
    val text = "one two three four five"
  }

  "resolve method" in {
    val text = "one two three four five."
    val substitutions = Seq(
      Substitution(Mention("one", 0), Mention("eleven", -1)),
      Substitution(Mention("two", 4), Mention("twelve", -1)),
      Substitution(Mention("three", 8), Mention("thirteen", -1)))

    import CoreferenceResolver._
    CoreferenceResolver.resolve(text, substitutions).map {
      case NormalString(string) => string
      case ResolvedString(actual, resolved) => resolved
    }.mkString("") must_== "eleven twelve thirteen four five."
    CoreferenceResolver.resolve(text, substitutions).map {
      case NormalString(string) => string
      case ResolvedString(actual, resolved) => actual
    }.mkString("") must_== "one two three four five."
  }

  "is possessivize" in {
    Mention.isPossessive("Schmitz's") must beTrue
    Mention.isPossessive("Schmitzes'") must beTrue
    Mention.isPossessive("my") must beTrue
    Mention.isPossessive("your") must beTrue
    Mention.isPossessive("his") must beTrue
    Mention.isPossessive("her") must beTrue
    Mention.isPossessive("their") must beTrue
    Mention.isPossessive("our") must beTrue

    Mention.isPossessive("Schmitz") must beFalse
    Mention.isPossessive("Schmits") must beFalse
  }

  "possessivize" in {
    Mention.possessivize("Schmitz") must_== "Schmitz's"
    Mention.possessivize("Schmitzes") must_== "Schmitzes'"
  }
}

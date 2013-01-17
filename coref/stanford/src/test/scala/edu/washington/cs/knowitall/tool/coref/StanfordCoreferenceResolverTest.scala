package edu.washington.cs.knowitall
package tool
package coref

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import edu.washington.cs.knowitall.collection.immutable.Interval

@RunWith(classOf[JUnitRunner])
object StanfordCoreferenceResolverTest extends Specification {
  "substitute method" in {
    val text = "one two three four five."
    val substitutions = Seq(
        Interval.open(0, 3) -> "eleven",
        Interval.open(4, 7) -> "twelve",
        Interval.open(8, 13) -> "thirteen"
      )

    CoreferenceResolver.substituteBase(text, substitutions) must_== "eleven twelve thirteen four five."
    CoreferenceResolver.substituteBase(text, substitutions.reverse) must_== "eleven twelve thirteen four five."
  }

  "resolve method" in {
    val text = "one two three four five."
    val substitutions = Seq(
        Substitution(Mention("one", 0), Mention("eleven", -1)),
        Substitution(Mention("two", 4), Mention("twelve", -1)),
        Substitution(Mention("three", 8), Mention("thirteen", -1))
      )

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

  "coreference works" in {
    val coref = new StanfordCoreferenceResolver()
    val text = "Michael went to the store.  He bought some eggs.  He married Lia.  She is a pharmacist."
    CoreferenceResolver.substitute(text, coref.substitutions(text)) must_== "Michael went to the store.  Michael bought some eggs.  Michael married Lia.  Lia is a pharmacist."
  }
}

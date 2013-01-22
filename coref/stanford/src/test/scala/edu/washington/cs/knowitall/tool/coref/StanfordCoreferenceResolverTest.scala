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
  "stanford coreference works" in {
    val coref = new StanfordCoreferenceResolver()
    val text = "Michael went to the store.  He bought some eggs.  He married Lia.  She is a pharmacist."
    CoreferenceResolver.substitute(text, coref.substitutions(text)) must_== "Michael went to the store.  Michael bought some eggs.  Michael married Lia.  Lia is a pharmacist."
  }

  "stanford coreference messes up possessives" in {
    val coref = new StanfordCoreferenceResolver()
    val text = "Michael wanted to go to the store but his car was broken."
    CoreferenceResolver.substitute(text, coref.substitutions(text)) must_== "Michael wanted to go to the store but Michael car was broken."
  }

  "stanford coreference messes up possessives but we fix them" in {
    val coref = new StanfordCoreferenceResolver()

    {
      val text = "Michael wanted to go to the store but his car was broken."
      val substitutions = coref.substitutions(text).map(_.fixPossessive)
      CoreferenceResolver.substitute(text, substitutions) must_== "Michael wanted to go to the store but Michael's car was broken."
    }

    {
      val text = "A gust of wind destroyed Michael's house.  He was very sad."
      val substitutions = coref.substitutions(text).map(_.fixPossessive)
      CoreferenceResolver.substitute(text, substitutions) must_== "A gust of wind destroyed Michael's house.  Michael was very sad."
    }
  }
}

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
}

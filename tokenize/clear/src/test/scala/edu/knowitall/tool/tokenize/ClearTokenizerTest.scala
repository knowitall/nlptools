package edu.knowitall
package tool
package tokenize

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object ClearTokenizerTest extends Specification {
  "tokenize example sentence" in {
    val text = "This is a test of Clear's tokenizer."
    val tokenizer = new ClearTokenizer()
    tokenizer(text).mkString(" ") must_== "This@0 is@5 a@8 test@10 of@15 Clear@18 's@23 tokenizer@26 .@35"
  }

  "tokenize pathological sentence" in {
    val text = "rough straight and"
    val tokenizer = new ClearTokenizer()
    tokenizer(text).mkString(" ") must_== "rough@0 straight@7 and@16"
  }
  
  "tokenize a sentence with unicode weirdness" in {
    val bytes = Array(112,	// p 
    				  117,	// u
    				  114,	// r
    				  112,	// p
    				  111,	// o
    				  115,	// s
    				  101,	// e
    				  32,	// space
    				  -62,	// not sure
    				  -105)	// not sure
    				  .map(_.asInstanceOf[Byte])
    val string = new String(bytes)
    val tokenizer = new ClearTokenizer()
    
    // Not sure what the expected output should be, but it should have at least
    // one token and not throw an exception.
    tokenizer(string).size must_!= 0
  }
}

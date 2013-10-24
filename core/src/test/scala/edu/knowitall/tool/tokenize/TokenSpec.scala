package edu.knowitall
package tool
package tokenize

import org.junit._
import org.junit.Assert._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.postag.Postagger

@RunWith(classOf[JUnitRunner])
object TokenSpecTest extends Specification {
  "tokens serialize and deserialize correctly" in {
    val token = Token("asdf", 0)
    Token.deserialize(token.serialize) == token
    
    val t = Token("asdf",0)
    val tSerialized = Token.serialization.write(t)
    val tDeserialized = Token.serialization.read(tSerialized)
    tDeserialized == t
    
    val pt = PostaggedToken("DT","in",3)
    PostaggedToken.serialization.read(PostaggedToken.serialization.write(pt)) == pt
  }
  
  "tokenizer serialization and deserialization work correctly" in {
    
    val token1 = Token("The",0)
    val token2 = Token("big",4)
    val tokens = Seq(token1,token2)
    val tokensSerialization = Tokenizer.serialization.write(tokens)
    Tokenizer.serialization.read(tokensSerialization) == tokens

  }
  
  "posTaggedTokenizer serialization and deserialization work correctly" in {
    val posToken1 = PostaggedToken("DT","The",0)
    val posToken2 = PostaggedToken("JJ","big",4)
    val posTokens = Seq(posToken1,posToken2)
    val posTokensSerialization = Postagger.serialization.write(posTokens)
    Postagger.serialization.read(posTokensSerialization) == posTokens
  }
  
  "deserializing Tokens from posTagger serialization works" in {
    val posToken1 = PostaggedToken("DT","The",0)
    val token1 = Token("The",0)
    val posToken2 = PostaggedToken("JJ","big",4)
    val token2 = Token("big",4)
    val posTokens = Seq(posToken1,posToken2)
    val tokens = Seq(token1,token2)
    val posTokensSerialization = Postagger.serialization.write(posTokens)
    Tokenizer.serialization.read(posTokensSerialization) == tokens
  }
}

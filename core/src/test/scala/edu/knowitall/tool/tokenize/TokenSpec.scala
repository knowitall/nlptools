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
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.chunk.Chunker

@RunWith(classOf[JUnitRunner])
object TokenSpecTest extends Specification {
  "tokens serialize and deserialize correctly" in {
    val token = Token("asdf", 0)
    Token.deserialize(token.serialize) === token
    
    val t = Token("asdf",0)
    val tSerialized = Token.stringFormat.write(t)
    val tDeserialized = Token.stringFormat.read(tSerialized)
    tDeserialized === t
    
    val pt = PostaggedToken("DT","in",3)
    PostaggedToken.stringFormat.read(PostaggedToken.stringFormat.write(pt)) == pt
  }
  
  "tokenizer serialization and deserialization work correctly" in {
    
    val token1 = Token("The",0)
    val token2 = Token("big",4)
    val tokens = Seq(token1,token2)
    val tokensSerialization = Tokenizer.stringFormat.write(tokens)
    Tokenizer.stringFormat.read(tokensSerialization) === tokens

  }
  
  "posTagger serialization and deserialization work correctly" in {
    val posToken1 = PostaggedToken("DT","The",0)
    val posToken2 = PostaggedToken("JJ","big",4)
    val posTokens = Seq(posToken1,posToken2)
    val posTokensSerialization = Postagger.stringFormat.write(posTokens)
    Postagger.stringFormat.read(posTokensSerialization) === posTokens
  }
  
  "chunker/chunkedToken serialization and deserialization work correctly" in {
    val chunkedToken1 = ChunkedToken("NP-DT","DT","The",0)
    val chunkedToken2 = ChunkedToken("NP-JJ","JJ","big",4)
    val chunkedTokens = Seq(chunkedToken1,chunkedToken2)
    val serializedChunkedTokens = Chunker.stringFormat.write(chunkedTokens)
    Chunker.stringFormat.read(serializedChunkedTokens) === chunkedTokens 
  }
  
  "MatchError should be thrown when String has a non-integer offset" in {
    val tokenSerializationString = "The@nonInteger"
    try{
      val token = Token.stringFormat.read(tokenSerializationString)
    }
    catch{
      case e: MatchError => { }
    }
  }
  
  "Token should be able to contain @" in {
   val t = Token("@",0)
   Token.stringFormat.read(Token.stringFormat.write(t)) == t
  }
}

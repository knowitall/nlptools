package edu.washington.cs.knowitall
package tool.chunk

import edu.knowitall.common.HashCodeHelper
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

/** A representation of a chunked token.  A chunked token has all the
  * aspects of a postagged token along with a chunk tag.
  *
  * @constructor
  * @param  string  the string of the token
  * @param  offset  the character offset of the token in the source sentence
  * @param  postag  the PENN-style part-of-speech tag of the token
  * @param  chunk   the chunk tag of the token in BIO format
  */
class ChunkedToken(val chunk: String, postag: String, string: String, offset: Int)
extends PostaggedToken(postag, string, offset) {
  def this(token: PostaggedToken, chunk: String) = this(chunk, token.postag, token.string, token.offset)

  override def toString = string+"/"+postag+"/"+chunk+"@"+offset

  override def hashCode = super.hashCode * 31 + HashCodeHelper(this.postag, this.chunk)
  def canEqual(that: ChunkedToken) = that.isInstanceOf[ChunkedToken]
  override def equals(that: Any) = that match {
    case that: ChunkedToken => (that canEqual this) &&
      super.equals(that) &&
      this.chunk == that.chunk
    case _ => false
  }
}

object ChunkedToken {
  def unapply(token: ChunkedToken): Option[(String, String, String, Int)] = Some((token.chunk, token.postag, token.string, token.offset))
}

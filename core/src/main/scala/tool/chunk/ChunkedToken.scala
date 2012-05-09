package edu.washington.cs.knowitall
package tool.chunk

import edu.washington.cs.knowitall.common.HashCodeHelper
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

class ChunkedToken(string: String, offset: Int, postag: String, val chunk: String)
extends PostaggedToken(string, offset, postag) {
  def this(token: PostaggedToken, chunk: String) = this(token.string, token.offset, token.postag, chunk)

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
package edu.washington.cs.knowitall
package tool.postag

import edu.washington.cs.knowitall.common.HashCodeHelper
import edu.washington.cs.knowitall.tool.tokenize.Token

class PostaggedToken(string: String, offset: Int, val postag: String)
extends Token(string, offset) {
  def this(token: Token, postag: String) = this(token.string, token.offset, postag)

  override def toString = string+"/"+postag+"@"+offset

  override def hashCode = super.hashCode * 31 + HashCodeHelper(this.postag)
  def canEqual(that: PostaggedToken) = that.isInstanceOf[PostaggedToken]
  override def equals(that: Any) = that match {
    case that: PostaggedToken => (that canEqual this) &&
      super.equals(that) &&
      this.postag == that.postag
    case _ => false
  }
}

object PostaggedToken {
  def unapply(token: PostaggedToken): Option[(String, String, Int)] = Some((token.postag, token.string, token.offset))
}
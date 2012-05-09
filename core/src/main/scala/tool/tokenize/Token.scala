package edu.washington.cs.knowitall
package tool
package tokenize

import edu.washington.cs.knowitall.common.HashCodeHelper

class Token(val string: String, val offset: Int) {
  override def toString = string + "@" + offset

  override def hashCode = HashCodeHelper(this.string, this.offset)
  def canEqual(that: Token) = that.isInstanceOf[Token]
  override def equals(that: Any) = that match {
    case that: Token => (that canEqual this) &&
      this.string == that.string &&
      this.offset == that.offset
    case _ => false
  }
}

object Token {
  def unapply(token: Token): Option[(String, Int)] = Some((token.string, token.offset))
}
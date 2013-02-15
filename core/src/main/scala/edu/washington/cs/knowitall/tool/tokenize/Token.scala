package edu.washington.cs.knowitall
package tool
package tokenize

import edu.washington.cs.knowitall.common.HashCodeHelper
import edu.washington.cs.knowitall.collection.immutable.Interval

/** The most simple representation of a token.  A token has a string
  * and a character offset in the original text.
  *
  * @param  string  the string of the token
  * @param  offset  the character offset of the token in the source sentence
  */
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
  
  @deprecated
  def interval = offsets
  
  def offsets = Interval.open(offset, offset + string.length)
}

object Token {
  def unapply(token: Token): Option[(String, Int)] = Some((token.string, token.offset))
}

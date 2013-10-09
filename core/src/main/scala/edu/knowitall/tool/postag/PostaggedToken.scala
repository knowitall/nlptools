package edu.knowitall
package tool.postag

import edu.knowitall.common.HashCodeHelper
import edu.knowitall.tool.tokenize.Token

/** A representation for a part-of-speech tagged token.  POS tokens
  * use PENN-treebank style tags.
  *
  * @param  string  the string of the token
  * @param  offset  the character offset of the token in the source sentence
  * @param  postag  the PENN-style part-of-speech tag of the token
  * @param  chunk   the chunk tag of the token in BIO format
  */
class PostaggedToken(val postag: String, override val string: String, override val offset: Int)
extends Token(string, offset) {
  def this(token: Token, postag: String) = this(postag, token.string, token.offset)

  override def toString = string+"/"+postag+"@"+offset

  override def hashCode = super.hashCode * 31 + HashCodeHelper(this.postag)
  def canEqual(that: PostaggedToken) = that.isInstanceOf[PostaggedToken]
  override def equals(that: Any) = that match {
    case that: PostaggedToken => (that canEqual this) &&
      super.equals(that) &&
      this.postag == that.postag
    case _ => false
  }

  def isProperNoun = postag == "NNP" || postag == "NNPS"
  def isCommonNoun = postag == "NN" || postag == "NNS"
  def isNoun = isProperNoun || isCommonNoun
  def isPluralNoun = postag == "NNS" || postag == "NNPS"

  def isVerbBase = postag == "VB"
  def isVerbPast = postag == "VBD"
  def isVerbGerund = postag == "VBG"
  def isVerbPastParticiple = postag == "VBN"
  def isVerbNon3pPresent = postag == "VBP"
  def isVerb3pPresent = postag == "VBZ"
  def isVerbPresent = isVerbNon3pPresent || isVerb3pPresent
  def isVerb = postag.startsWith("VB")

  def isPlainAdjective = postag == "JJ"
  def isComparativeAdjective = postag == "JJR"
  def isSuperlativeAdjective = postag == "JJS"
  def isAdjective = isPlainAdjective || isComparativeAdjective || isSuperlativeAdjective

  def isPersonalPronoun = postag == "PRP"
  def isPossessivePronoun = postag == "PRP$"
  def isPronoun = isPersonalPronoun || isPossessivePronoun

  def isPossessive = isPossessivePronoun || postag == "POS"

  def isDeterminer = postag == "DT"
  def isCardinalNumber = postag == "CD"
  def isSuperlativeAdverb = postag == "RBS"
  def isPunctuation = punctuation.contains(postag)
  def isSubordinatingConjunction = postag == "IN"
  def isCoordinatingConjunction = postag == "CC"
  def isConjunction = isSubordinatingConjunction || isCoordinatingConjunction
  def isPreposition = postag == "IN"

  def isWhDeterminer = postag == "WDT"
  def isWhPronoun = postag == "WP"
  def isWhPossessivePronoun = postag == "WP$"
  def isWhAdverb = postag == "WRB"
  def isWhWord = isWhDeterminer || isWhPronoun || isWhPossessivePronoun || isWhAdverb

  val punctuation = Set("#", "$", "''", "(", ")", ",", ".", ":", "``")
}

object PostaggedToken {
  def unapply(token: PostaggedToken): Option[(String, String, Int)] = Some((token.postag, token.string, token.offset))
}

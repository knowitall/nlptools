package edu.washington.cs.knowitall
package tool
package stem

import tool.tokenize.Token

case class Lemmatized[T <: Token](token: T, lemma: String)

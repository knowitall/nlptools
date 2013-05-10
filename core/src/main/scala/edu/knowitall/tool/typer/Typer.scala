package edu.knowitall.tool.typer

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.tokenize.Token

abstract class Typer[E <: Token](val name: String, val source: String) {
  def apply(seq: Seq[E]): Seq[Type]
}

case class Type(val name: String, val source: String, val tokenInterval: Interval, val text: String) {
  def matchText[E <: Token](seq: Seq[E]): String = seq.iterator.slice(tokenInterval.start, tokenInterval.end).map(_.string).mkString(" ")

  def tokens[E <: Token](seq: Seq[E]): Seq[E] = seq.slice(tokenInterval.start, tokenInterval.end)
}

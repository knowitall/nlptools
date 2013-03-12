package edu.washington.cs.knowitall.tool.typer

import edu.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.tokenize.Token

abstract class Typer[E <: Token](val name: String, val source: String) {
  def apply(seq: Seq[E]): Seq[Type]
}

case class Type(val name: String, val source: String, val interval: Interval, val text: String) {
  def matchText[E <: Token](seq: Seq[E]): String = seq.iterator.slice(interval.start, interval.end).map(_.string).mkString(" ")

  def tokens[E <: Token](seq: Seq[E]): Seq[E] = seq.slice(interval.start, interval.end)
}

package edu.washington.cs.knowitall
package tool
package parse

import scala.collection.Iterable

class MultiIterable[E](val iterables: Iterable[E]*) extends Iterable[List[E]] {
  def iterator = new MultiIterator(iterables.map(_.iterator): _*)
}

class MultiIterator[E](private val iterators: Iterator[E]*) extends Iterator[List[E]] {
  def hasNext = iterators.forall(_.hasNext)
  def next = iterators.map(_.next).toList
}

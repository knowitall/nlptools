package edu.knowitall.tool

trait Writer[F, T] {
  def write(from: F): T
}

trait Reader[F, T] {
  def read(from: F): T
}

trait Format[F, T]
extends Writer[F, T] with Reader[T, F]

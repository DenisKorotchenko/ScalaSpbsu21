package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

case object IntNil extends IntList[Nothing] {
  override def head: Nothing = undef
  override def tail: IntList[Nothing] = IntNil
  override def drop(n: Int): IntList[Nothing] = if (n == 0) IntNil else undef
  override def take(n: Int): IntList[Nothing] = if (n == 0) IntNil else undef
  override def map[T1](f: Nothing => T1): IntList[T1] = IntNil
}

case class IntCons[+T](head: T, tail: IntList[T]) extends IntList[T] {
  override def drop(n: Int): IntList[T] = if (n == 0) this else tail.drop(n-1)
  override def take(n: Int): IntList[T] = if (n == 0) IntNil else IntCons(head, tail.take(n - 1))
  override def map[T1](f: T => T1): IntList[T1] = IntCons(f(head), tail.map(f))
}

sealed trait IntList[+T] {
  def head: T
  def tail: IntList[T]
  def drop(n: Int): IntList[T]
  def take(n: Int): IntList[T]
  def map[T1](f: T => T1): IntList[T1]
  def ::[T1 >: T](elem: T1): IntList[T1] = IntCons(elem, this)
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): IntList[T] = if (seq.isEmpty) IntNil else seq.head :: fromSeq(seq.tail)
  def sum(intList: IntList[Int]): Int      = intList match {
    case IntNil => undef
    case IntCons(head, IntNil) => head
    case IntCons(head, tail) => head + sum(tail)
  }
  def size[T](intList: IntList[T]): Int     = intList match {
    case IntNil => 0
    case IntCons(_, tail) => 1 + size(tail)
  }
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}
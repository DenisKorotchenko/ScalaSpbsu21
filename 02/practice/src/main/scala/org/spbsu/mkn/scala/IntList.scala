package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

case object IntNil extends IntList {
  override def head: Int = undef
  override def tail: IntList = IntNil
  override def drop(n: Int): IntList = if (n == 0) IntNil else undef
  override def take(n: Int): IntList = if (n == 0) IntNil else undef
  override def map(f: Int => Int): IntList = IntNil
}

case class IntCons(head: Int, tail: IntList) extends IntList {
  override def drop(n: Int): IntList = if (n == 0) this else tail.drop(n-1)
  override def take(n: Int): IntList = if (n == 0) IntNil else IntCons(head, tail.take(n - 1))
  override def map(f: Int => Int): IntList = IntCons(f(head), tail.map(f))
}

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = IntCons(elem, this)
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = if (seq.isEmpty) IntNil else seq.head :: fromSeq(seq.tail)
  def sum(intList: IntList): Int      = intList match {
    case IntNil => undef
    case IntCons(head, IntNil) => head
    case IntCons(head, tail) => head + sum(tail)
  }
  def size(intList: IntList): Int     = intList match {
    case IntNil => 0
    case IntCons(_, tail) => 1 + size(tail)
  }
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}
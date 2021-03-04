package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyList._

case object MyNil extends MyList[Nothing] {
  override def head: Nothing = undef
  override def tail: MyList[Nothing] = MyNil
  override def drop(n: Int): MyList[Nothing] = if (n == 0) MyNil else undef
  override def take(n: Int): MyList[Nothing] = if (n == 0) MyNil else undef
  override def map[T1](f: Nothing => T1): MyList[T1] = MyNil
}

case class MyCons[+T](head: T, tail: MyList[T]) extends MyList[T] {
  override def drop(n: Int): MyList[T] = if (n == 0) this else tail.drop(n-1)
  override def take(n: Int): MyList[T] = if (n == 0) MyNil else MyCons(head, tail.take(n - 1))
  override def map[T1](f: T => T1): MyList[T1] = MyCons(f(head), tail.map(f))
}

sealed trait MyList[+T] {
  def head: T
  def tail: MyList[T]
  def drop(n: Int): MyList[T]
  def take(n: Int): MyList[T]
  def map[T1](f: T => T1): MyList[T1]
  def ::[T1 >: T](elem: T1): MyList[T1] = MyCons(elem, this)
}

object MyList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyList[T] = if (seq.isEmpty) MyNil else seq.head :: fromSeq(seq.tail)
  def sum(intList: MyList[Int]): Int      = intList match {
    case MyNil => undef
    case MyCons(head, MyNil) => head
    case MyCons(head, tail) => head + sum(tail)
  }
  def size[T](intList: MyList[T]): Int     = intList match {
    case MyNil => 0
    case MyCons(_, tail) => 1 + size(tail)
  }
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}
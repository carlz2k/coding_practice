package com.carlz2k.pure.model

import scala.annotation.tailrec

/**
  * <i>Copyright (c) 2018, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class ListExercise[T] {
  def last(list: List[T]): Option[T] = {
    list match {
      case head :: Nil => Option(head)
      case head :: tail => last(tail)
      case _ => Option.empty
    }
  }

  def penultimate(list: List[T]): Option[T] = {
    list match {
      case h :: _ :: Nil => Option(h)
      case _ :: tail => penultimate(tail)
      case _ => Option.empty
    }
  }

  def findNth(n: Int, ls: List[T]): Option[T] = (n, ls) match {
    case (0, h :: _) => Option(h)
    case (nthElementIndex, _ :: tail) => findNth(nthElementIndex - 1, tail)
    case (_, Nil) => Option.empty
  }

  def testUnderscore(list: List[T]): Option[T] = list match {
    case h :: b :: _ => Option(b)
    case Nil => Option.empty
  }

  def length(list: List[T]): Int = {
    list.foldLeft(0)((sum, _) => sum + 1)
  }

  def reverse(list: List[T]): List[T] =
    list.foldLeft(List[T]())((r, h) => h :: r)

  def flatten(ls: List[List[T]]): List[T] = ls match {
    case (h: List[T]) :: (tail: List[List[T]]) => h ::: flatten(tail)
    case List() => List()
  }

  def flatten2(ls: List[List[T]]): List[T] = ls flatMap {
    case h: List[List[T]] => flatten(h)
    case h: T => List(h)
  }

  def compress(ls: List[T]): List[T] =
    ls.foldRight(List[T]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }

  def pack(ls: List[T]): List[List[T]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span {
        _ == ls.head
      }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode(ls: List[T]): List[(Int, T)] = {
    pack(ls).map({
      x => (x.length, x.head)
    })
  }

  def encodeModified2(ls: List[T]): List[Either[T, (Int, T)]] = {
    encode(ls) map {
      t => if (t._1 == 0) Left(t._2) else Right(t)
    }
  }

  def decode(ls: List[(Int, T)]): List[T] =
    ls flatMap { e => List.fill(e._1)(e._2) }

  def duplicateN(n: Int, ls: List[T]): List[T] = {
    ls flatMap (a => List.fill(n)(a))
  }

  def drop(n: Int, ls: List[T]): List[T] = {
    @tailrec
    def dropR(index: Int, curList: List[T], result: List[T]): List[T] = (index, curList) match {
      case (_, Nil) => result
      case (i, h :: tail) =>
        if (i % n == (n - 1)) {
          dropR(i + 1, tail, result)
        } else {
          dropR(i + 1, tail, result :+ h)
        }
    }

    dropR(0, ls, List())
  }

  def split(n: Int, ls: List[T]): (List[T], List[T]) =
    (ls.take(n), ls.drop(n))

  def slice(s: Int, e: Int, ls: List[T]): List[T] = {
    ls drop s take ((e - s) max 0)
  }

  def rotate(n: Int, ls: List[T]): List[T] = {
    if (n >= 0) {
      (ls drop n) ::: (ls take n)
    } else {
      val length = ls.length - (n * -1)
      (ls drop length) ::: (ls take length)
    }
  }

  def removeAt(n: Int, ls: List[T]): (List[T], T) = {
    @tailrec
    def removeAtRecursive(counter: Int, result: List[T], curList: List[T], element: Option[T]): (List[T], T) = {

      (counter, result, curList, element) match {
        case (_, _, Nil, _) => (result, element.get)
        case (i, r, h :: tail, _) =>
          if (i == n) {
            removeAtRecursive(i + 1, r, tail, Option(h))
            //(result ::: tail, h)
          } else {
            removeAtRecursive(i + 1, r :+ h, tail, element)
          }
      }
    }

    removeAtRecursive(0, List(), ls, Option.empty)
  }

  def combinations(n: Int, ls: List[T]): List[List[T]] = {
    if (n > ls.length) Nil
    else ls match {
      case _ :: _ if n == 1 => ls.map(List(_))
      case h :: tl => combinations(n - 1, tl).map(h :: _) ::: combinations(n, tl)
      case _ => Nil
    }
  }

  def add(x: Int): Int => Int = {
    (y: Int) => x + y
  }

  def addOne: Int => Int = {
    (x: Int) => x + 1
  }
}

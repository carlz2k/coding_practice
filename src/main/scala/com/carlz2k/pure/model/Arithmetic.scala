package com.carlz2k.pure.model

/**
  * <i>Copyright (c) 2018, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class Arithmetic(val start: Int) {

  import Arithmetic._

  def isPrime: Boolean =
    (start > 1) && (primes takeWhile {
      _ <= Math.sqrt(start)
    } forall {
      start % _ != 0
    })

  def goldbach: (Int, Int) =
    primes takeWhile {
      _ < start
    } find { p => (start - p).isPrime } match {
      case None => throw new IllegalArgumentException
      case Some(p1) => (p1, start - p1)
    }
}

object Arithmetic {
  implicit def int2Arithmetic(i: Int): Arithmetic = new Arithmetic(i)

  // P31
  val primes = Stream.cons(2, Stream.from(3, 2) filter {
    _.isPrime
  })

  // P41
  def printGoldbachList(r: Range) {
    printGoldbachListLimited(r, 0)
  }

  // P41
  def printGoldbachListLimited(r: Range, limit: Int) {
    (r filter { n => n > 2 && n % 2 == 0 } map { n => (n, n.goldbach) }
      filter {
      _._2._1 >= limit
    } foreach {
      _ match {
        case (n, (p1, p2)) => println(n + " = " + p1 + " + " + p2)
      }
    })
  }
}

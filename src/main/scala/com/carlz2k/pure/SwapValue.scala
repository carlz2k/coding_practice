package com.carlz2k.pure

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
case class SwapValue() {
  def swap(a: Integer, b: Integer): Unit = {
    println("before swap")
    println("a = " + a)
    println("b = " + b)

    var tempA = a
    var tempB = b
    tempA ^= tempB
    tempB ^= tempA
    tempA ^= tempB

    println("after swap")
    println("a = " + tempA)
    println("b = " + tempB)
  }
}

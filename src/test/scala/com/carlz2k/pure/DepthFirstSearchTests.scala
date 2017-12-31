package com.carlz2k.pure

import org.specs2.Specification

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class DepthFirstSearchTests extends Specification {

  def is =
    s2"""

  This is a specification for check if coordinates form a square

  The check coordinates should
    return true                            $test
                                                      """

  def test: Boolean = {
    val g = Map(1 -> List(2, 4), 2 -> List(1, 3), 3 -> List(2, 4), 4 -> List(1, 3))

    println(List(2).flatMap(g))

    println(List(2).map(g))

    println(List(List(3)).flatten)

    true
  }

}

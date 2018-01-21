package com.carlz2k.pure

import com.carlz2k.pure.model.ListExercise
import org.specs2.Specification

/**
  * <i>Copyright (c) 2018, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class ListExerciseTests extends Specification{
  def is =
    s2"""

  This is a specification for check how underscore works in scala

  The check should
    return true                            $test
                                                      """

  def test: Boolean = {
    val listFind = new ListExercise[String]()
    println(listFind.testUnderscore(List("1", "2", "3")))
    println(listFind.length(List("1", "2", "3")))
    println(listFind.reverse(List("1", "2", "3")))
    //println(listFind.flatten(List(List("1", "2"), List("2", "3"), List("5"))))
    //println(listFind.flatten2(List(List("1", "2"), List("2", "3"), List("5"))))
    println(listFind.pack(List("a", "a", "a", "b", "c", "c")))
    println(listFind.encode(List("a", "a", "a", "b", "c", "c")))
    println(listFind.duplicateN(3, List("a", "b", "c", "c", "d")))
    println(listFind.drop(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")))
    println(listFind.slice(3, 7, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")))
    println(listFind.rotate(5, List()))
    println(listFind.rotate(3, List("a", "b", "c", "d", "e", "f", "g", "h")))
    println(listFind.rotate(-3, List("a", "b", "c", "d", "e", "f", "g", "h")))
    println(listFind.removeAt(2, List("a", "b", "c", "d", "e")))

    println(listFind.combinations(3, List("a", "b", "c", "d", "e")))
    true
  }
}

package com.carlz2k.pure

import org.specs2.Specification

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class BuddyBitMapTests extends Specification {
  def is =
    s2"""

  This is a specification for check if coordinates form a square

  The buddy map system should
    clearbit return true                            $testClearTrue
    setbit return true                            $testSetTrue
                                                      """

  def testClearTrue: Boolean = {
    val tree = Array(0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1)
    val buddyBitMap = BuddyBitMap(tree)
    buddyBitMap.clearBit(2, 6)
    buddyBitMap.tree sameElements Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)

    val tree2 = Array(0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0)
    val buddyBitMap2 = BuddyBitMap(tree2)
    buddyBitMap2.clearBit(6, 1)
    buddyBitMap2.tree sameElements Array(0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0)
  }

  def testSetTrue: Boolean = {
    val tree = Array(0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1)
    val buddyBitMap = BuddyBitMap(tree)
    buddyBitMap.setBit(2, 1)
    buddyBitMap.tree sameElements Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    val tree2 = Array(0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0)
    val buddyBitMap2 = BuddyBitMap(tree2)
    buddyBitMap2.setBit(6, 1)
    buddyBitMap2.tree sameElements Array(0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1)
  }
}

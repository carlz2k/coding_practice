package com.carlz2k.pure

import com.carlz2k.pure.model.Coordinate
import org.specs2.Specification

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class BitOperatorTests extends Specification {
  def is =
    s2"""

  This is a specification for right shift bit operator

  The result should
    signed right shift return true                            $testSignedRightShift
    unsigned right shift return true                          $testunsignedRightShift
                                                      """

  def testSignedRightShift: Boolean = {

    println(0^5^5)
    println(0^5^5^3)
    println(5^3^5)
    var a = 120
    a = a >> 2
    Integer.toBinaryString(a) == "11110"
    var b = -120
    b = b >> 3
    Integer.toBinaryString(b) == "11111111111111111111111111110001"
  }

  def testunsignedRightShift: Boolean = {
    var a = 120
    a = a >>> 2
    Integer.toBinaryString(a) == "11110"

    var b = -120
    b = b >>> 3
    Integer.toBinaryString(b) == "11111111111111111111111110001"
  }
}

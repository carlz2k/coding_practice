package com.carlz2k.pure

import org.specs2.Specification

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class SwapValueTests extends Specification {
  def is =
    s2"""

  This is a specification for swap two integers

  The result should
    return true                            $testTrue
                                                      """

  def testTrue: Boolean = {
    SwapValue().swap(5, 6)

    true
  }
}

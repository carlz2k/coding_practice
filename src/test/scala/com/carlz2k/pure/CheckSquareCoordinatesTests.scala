package com.carlz2k.pure

import org.specs2.Specification
import com.carlz2k.pure.model.Coordinate

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
class CheckSquareCoordinatesTests extends Specification {
  def is =
    s2"""

  This is a specification for check if coordinates form a square

  The check coordinates should
    return true                            $testTrue
                                                      """

  def testTrue: Boolean = {
    val coordinates1 =
      List(Coordinate(1, 2), Coordinate(2, 2), Coordinate(2, 1), Coordinate(1, 1))
    CheckSquareCoordinates.checkFormSquare(coordinates1)

    val coordinates2=
      List(Coordinate(1, 1), Coordinate(2, 2), Coordinate(2, 1), Coordinate(1, 2))
    CheckSquareCoordinates.checkFormSquare(coordinates2)
  }
}
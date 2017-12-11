package com.carlz2k.pure

import com.carlz2k.pure.model.Coordinate

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
object CheckSquareCoordinates {
  def sum(l: List[Int]): Int = l.sum

  def checkFormSquare(coordinates: List[Coordinate]): Boolean
  = {
    val p1 = coordinates(0)
    val p2 = coordinates(1)
    val p3 = coordinates(2)
    val p4 = coordinates(3)

    val distance1 = p1.distanceSquare(p2)
    val distance2 = p1.distanceSquare(p3)
    val distance3 = p1.distanceSquare(p4)

    if (distance1 == distance2 && (distance1 + distance2) == distance3) {
      val distance4 = p4.distanceSquare(p2)
      val distance5 = p4.distanceSquare(p3)

      return distance4 == distance5 && distance4 == distance1
    }

    if (distance2 == distance3 && (distance2 + distance3) == distance1) {
      val distance4 = p2.distanceSquare(p3)
      val distance5 = p2.distanceSquare(p4)

      return distance4 == distance5 && distance4 == distance2

    }

    if (distance1 == distance3 && (distance1 + distance3) == distance2) {
      val distance4 = p3.distanceSquare(p2)
      val distance5 = p3.distanceSquare(p4)

      return distance4 == distance5 && distance4 == distance1
    }

    false
  }
}



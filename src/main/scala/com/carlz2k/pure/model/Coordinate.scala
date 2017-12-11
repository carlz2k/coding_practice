package com.carlz2k.pure.model

case class Coordinate(x: Int, y: Int) {
  def distanceSquare(coordinate: Coordinate): Int = {
    (coordinate.x - x) * (coordinate.x - x) + (coordinate.y - y) * (coordinate.y - y)
  }
}



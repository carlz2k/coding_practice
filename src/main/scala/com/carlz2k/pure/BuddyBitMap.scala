package com.carlz2k.pure

import scala.annotation.tailrec

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
case class BuddyBitMap(tree: Array[Int]) {

  def setBit(offset: Int, length: Int): Unit = {
    val upperBound = Math.min(tree.length, offset + length)
    for (i <- offset to upperBound) {
      if (tree(i - 1) != 1) {
        tree(i - 1) = 1
        updateChildren(i, 1)
        updateParents(i, 1)
      }
    }
  }

  def clearBit(offset: Int, length: Int): Unit = {
    val upperBound = Math.min(tree.length, offset + length)

    for (i <- offset to upperBound) {
      if (tree(i - 1) != 0) {
        tree(i - 1) = 0
        updateChildren(i, 0)
        updateParents(i, 0)
      }
    }
  }

  private def getParentOffset(childOffset: Int): Int = {
    childOffset / 2
  }

  @tailrec
  private def updateParents(i: Int, value: Int): Unit = {

    if (i > 1) {
      val parentOffset = getParentOffset(i)
      var right = 0
      var left = 0
      if (i % 2 == 1) {
        right = tree(i - 1)
        left = tree(i - 2)
      } else {
        left = tree(i - 1)
        if (i < tree.length) {
          right = tree(i)
        } else {
          right = value // not right children, parent value will depend on the left child value
        }
      }
      tree(parentOffset-1) = left & right

      if (parentOffset>1) {
        updateParents(parentOffset, value)
      }
    }
  }

  private def updateChildren(i: Int, value: Int): Unit = {
    val leftOffset = i * 2
    val rightOffset = i * 2 + 1
    var originalLeft = value
    var originalRight = value

    if (leftOffset <= tree.length) {
      originalLeft = tree(leftOffset - 1)
      tree(leftOffset - 1) = value
    }

    if (rightOffset <= tree.length) {
      originalRight = tree(rightOffset - 1)
      tree(rightOffset - 1) = value
    }

    if (originalLeft != value) {
      updateChildren(leftOffset, value)
    }

    if (originalRight != value) {
      updateChildren(rightOffset, value)
    }
  }

  def printTree(): Unit = {
    for (i <- tree.indices) {
      print(tree(i) + ",")
    }
  }
}

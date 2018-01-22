package com.carlz2k.pure.model

/**
  * <i>Copyright (c) 2018, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description 
  */
case class GateToWorld[A](a: A) {
  def flatMap[B](f: A => GateToWorld[B]): GateToWorld[B] = f(a)

  def map[B](f: A => B): GateToWorld[B] = this.flatMap(x => GateToWorld(f(x)))
}

package frog

trait WhatWant extends Compound {
  val name: String
  def toSimpleString: String = s"$name?"
}

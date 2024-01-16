package frog

class Solving(
  val bind: Map[WhatWant, Compound | Null],
  val goal: Goal
) extends Solver {

}
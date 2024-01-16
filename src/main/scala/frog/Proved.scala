package frog
class Proved (
   override val bind: Map[WhatWant, Compound],
   override val compound: Compound
) extends Prover {
  implicit def toSolver: Solved = Solved(bind, compound)
  
}

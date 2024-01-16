package frog
import scala.collection.mutable

class Proving(
   override val bind: Map[WhatWant, Compound | Null],
   override val compound: Compound
) extends Prover {
  implicit def toSolver: Solving = Solving(bind, compound)
}

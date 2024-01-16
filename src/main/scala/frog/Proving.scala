package frog
import scala.collection.mutable

class Proving(
   override val bind: Map[What, Compound | Null],
   override val goal: Goal
) extends Prover

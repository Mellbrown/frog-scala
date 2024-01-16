package frog
class Proved (
   override val bind: Map[What, Compound],
   override val compound: Compound
) extends Prover {
  implicit def toSolver: Solved = Solved(bind, compound)
  
}

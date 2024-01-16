package frog

implicit class Facts (
   val facts: List[Proved] = List()
){
  def + (compound: Compound) : Facts = Facts(facts :+ compound.proved)
  def <+>? (goal: Goal): Solutions = this.solve(goal)
  def solve (goal: Goal): Solutions = goal.solveFrom(this)
  def prove (compound: Compound): Facts = compound.proveFrom(this)
  override def toString: String = "<+>\n + " + facts.mkString("\n + ")
}

object Facts {
  def apply (proved: Proved*): Facts = new Facts(proved.toList)
}
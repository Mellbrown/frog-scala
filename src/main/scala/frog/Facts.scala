package frog

implicit class Facts (val facts: List[Proved] = List()){
  def + (compound: Compound) : Facts = Facts(facts :+ compound.proved)
  def <+>? (goal: Goal): Facts = this.prove(goal)
  def prove (goal: Goal): Facts = goal.proveFrom(this)
  override def toString: String = "<+>\n + " + facts.mkString("\n + ")
}

object Facts {
  def apply (proved: Proved*): Facts = new Facts(proved.toList)
}
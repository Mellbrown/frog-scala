package frog

implicit class Facts (val facts: List[Proved] = List()){
  def + (compound: Compound) : Facts = Facts(facts :+ compound.proved)
  def <+>? (compound: Compound): Facts = this.prove(compound)
  def prove (compound: Compound): Facts = compound.proveFrom(this)
  override def toString: String = "<+>\n + " + facts.mkString("\n + ")
}

object Facts {
  def apply (proved: Proved*): Facts = new Facts(proved.toList)
}
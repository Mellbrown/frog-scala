package frog

class What(
  override val name: String,
) extends WhatWant {

  override def proveFrom(facts: Facts): Facts = 
    facts.facts.map(fact => Proved(
      Map(this -> fact.compound),
      fact.compound
    ))
  
  override def equals(obj: Any): Boolean = {
    obj match
      case what: What => name == what.name
      case _ => false
  }
  override def toString: String = s"$name?"

  override def hashCode(): Int = if (name != "") name.hashCode else super.hashCode

  def apply (compounds: Compound*) : WhatTerm = WhatTerm(name, compounds*)
  
}

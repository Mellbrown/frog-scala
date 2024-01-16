package frog

implicit class Atom(val atom: String | Number | Boolean) extends Compound {
  
  override def proveFrom(facts: Facts): Facts = facts.facts.filter(fact => this == fact.compound)

  override def equals(obj: Any): Boolean = {
    obj match
      case atom0: Atom => atom == atom0.atom
      case _ => false
  }
  override def toString: String = atom.toString
}

package frog

class WhatTerm(
  override val name: String,
  val compounds: Compound*
) extends WhatWant {

  override def proveFrom(facts: Facts): Facts = {
    facts.facts.filter(proved => {
      proved.compound match
        case term: Term =>
            compounds.length == term.compounds.length
        case _ => false
    }).flatMap(proved => {
      proved.compound match
        case term: Term =>
          compounds.zipWithIndex.map(
            (compound, index) => compound.proveFrom(term.compounds(index)).facts
          ).foldLeft[List[Proved]](
            List(Proved(Map(this -> term.predicate), term.predicate))
          )((A, B) => {
            A.facts.flatMap(a => {
              B.facts.flatMap(b => {
                (a << b).optional
              })
            })
          }).map(proved => Proved(proved.bind, term))
    })
  }
  override def equals(obj: Any): Boolean = {
    obj match
      case what: WhatTerm => name == what.name
      case _ => false
  }
  override def toString: String = s"$name?(${compounds.mkString(", ")})"

  override def hashCode(): Int = if (name != "") name.hashCode else super.hashCode
  
}

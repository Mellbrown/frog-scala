package frog

import scala.util.control.Breaks.{break, breakable}

class Term (
  val predicate: frog.Predicate,
  val compounds: Compound*
) extends Compound {
  override def proveFrom(facts: Facts): Facts = {
    facts.facts.filter(proved => {
      proved.goal match
        case term: Term =>
          predicate == term.predicate &&
            compounds.length == term.compounds.length
        case _ => false
    }).flatMap(proved => {
      proved.goal match
        case term: Term =>
          compounds.zipWithIndex.map(
            (compound, index) => compound.proveFrom(term.compounds(index)).facts
          ).reduce[List[Proved]]((A, B) => {
            A.facts.flatMap(a => {
              B.facts.flatMap(b => {
                (a<<b).optional
              })
            })
          }).map(proved => Proved(proved.bind, term))
    })
  }
  override def toString: String = s"$predicate${compounds.mkString(", ")})"
}

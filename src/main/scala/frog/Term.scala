package frog

import scala.util.control.Breaks.{break, breakable}

class Term (
  val predicate: frog.Predicate,
  val compounds: Compound*
) extends Compound {
  override def proveFrom(facts: Facts): Facts = {
    facts.facts.filter(proved => {
      proved.proved match
        case term: Term =>
          predicate == term.predicate &&
            compounds.length == term.compounds.length
        case _ => false
    }).flatMap(proved => {
      proved.proved match
        case term: Term =>
          compounds.zipWithIndex.map(
            (compound, index) => compound.proveFrom(term.compounds(index)).facts
          ).reduce[List[Proved]]((A, B) => {
            A.facts.flatMap(a => {
              B.facts.flatMap(b => {
                var is_mergeable = true
                breakable {
                  for ((key, value) <- a.bind) {
                    if (b.bind.contains(key)) {
                      if (value != b.bind(key)) {
                        is_mergeable = false
                        break
                      }
                    }
                  }
                }
                if (is_mergeable) {
                  List(Proved(a.bind ++ b.bind, term))
                } else {
                  List[Proved]()
                }
              })
            })
          }).map(proved => Proved(proved.bind, term))
    })
  }

  override implicit def proved: Proved = Proved(Map(), this)
  override def toString: String = s"$predicate${compounds.mkString(", ")})"


}

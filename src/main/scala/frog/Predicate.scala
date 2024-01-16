package frog

class Predicate (val predicate: String) extends Compound {
  override def equals(obj: Any): Boolean = {
    obj match
      case predicate0: Predicate => predicate == predicate0.predicate
      case string: String => predicate == string
      case _ => false
  }

  override def toString: String = s"$predicate("

  override def proveFrom(facts: Facts): Facts = {
    facts.facts.filter(proved => {
      proved.compound match
        case term: Term =>
          predicate == term.predicate.predicate
        case _ => false
    })
  }
}

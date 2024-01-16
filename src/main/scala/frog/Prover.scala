package frog

import scala.collection.mutable

trait Prover {
  val bind: Map[What, Compound]
  val goal: Goal
  def is_proved: Boolean = bind.forall(item => item._2 != null)
  implicit def optional : List[Proved] = if (is_proved) List(Proved(bind, goal)) else List()
  def <<(prover: Prover): Proving = {
    val m = mutable.Map.newBuilder[What, Compound | Null].addAll(bind).result
    prover.bind.foreach((key, value) => {
      m.put(key,
        if (m.contains(key))
          if (m(key) == value) value
          else null
        else value
      )
    })

    def compounds(compound: Compound): Seq[Compound] = {
      compound match
        case term: Term =>
          if (term.predicate.equals("")) term.compounds
          else Seq(term)
        case compound => Seq(compound)
    }
    
    Proving(m.toMap, goal && prover.goal)
  }

  override def toString: String =
    goal.toString + "\t" +
      bind.toList.map(
        (what, value) => s"$what=$value"
      ).mkString("\t")
}

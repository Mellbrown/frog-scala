package frog

trait Compound extends Goal {
  implicit def proved: Proved = Proved(Map(), this)
  def -- (compound: Compound): Compound = {
    def compounds(compound: Compound): Seq[Compound] = {
      compound match
        case term: Term =>
          if (term.predicate.equals("")) term.compounds
          else Seq(term)
        case compound => Seq(compound)
    }
    
    ""(compounds(this) ++ compounds(compound) *)
  }

  def proveFrom(facts: frog.Facts): frog.Facts
  override def solveFrom(facts: Facts): Solutions = proveFrom(facts)
}

package frog

implicit class Predicate (val predicate: String) {
  def apply (compounds: Compound*) : Term = Term(this, compounds*)

  override def equals(obj: Any): Boolean = {
    obj match
      case predicate0: Predicate => predicate == predicate0.predicate
      case _ => false
  }

  override def toString: String = s"$predicate("
}

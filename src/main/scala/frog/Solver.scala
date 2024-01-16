package frog

import scala.collection.mutable

trait Solver {
  val bind: Map[What, Compound]
  val goal: Goal
  def is_solved: Boolean = bind.forall(item => item._2 != null)
  implicit def optional: List[Solved] = if (is_solved) List(Solved(bind, goal)) else List()
  def <<(solver: Solver): Solving = {
    val m = mutable.Map.newBuilder[What, Compound | Null].addAll(bind).result
    solver.bind.foreach((key, value) => {
      m.put(key,
        if (m.contains(key))
          if (m(key) == value) value
          else null
        else value
      )
    })
    
    Solving(m.toMap, goal && solver.goal)
  }

  override def toString: String =
    goal.toString + "\t" +
      bind.toList.map(
        (what, value) => s"$what=$value"
      ).mkString("\t")
}

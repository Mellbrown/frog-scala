package frog

class AndGoal(val goals: Goal*) extends Goal {
  override def solveFrom(facts: Facts): Solutions = {
    goals
      .map(goal => goal.solveFrom(facts))
      .reduce((A, B) => {
        A.solutions.flatMap(a => {
          B.solutions.flatMap(b => {
            (a << b).optional
          })
        })
      })
  }

  override def toString: String = goals.mkString(" && ")
}
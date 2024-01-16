package frog

trait Goal {
  def &&(goal: Goal): AndGoal = {
    def goals(goal: Goal): Seq[Goal] = {
      goals match
        case andGoal: AndGoal => andGoal.goals
        case _ => Seq(goal)
    }

    AndGoal(goals(this) ++ goals(goal) *)
  }

  def solveFrom(facts: frog.Facts): Solutions
}
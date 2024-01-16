package frog

class AndGoal(val goals: Goal*) extends Goal {
  override def proveFrom(facts: Facts): Facts = facts
}
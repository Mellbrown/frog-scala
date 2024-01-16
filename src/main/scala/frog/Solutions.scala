package frog

implicit class Solutions(val solutions: List[Solved] = List()) {

  override def toString: String = "<+>\n + " + solutions.mkString("\n + ")
}

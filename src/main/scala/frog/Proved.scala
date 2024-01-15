package frog

class Proved (
   val bind: Map[What, Compound],
   val proved: Compound
) {

  override def toString: String = 
    proved.toString + "\t" + 
      bind.toList.map(
        (what, value) => s"$what=$value"
      ).mkString("\t")
}

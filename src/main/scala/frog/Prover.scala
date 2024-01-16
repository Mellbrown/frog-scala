package frog

import scala.collection.mutable

trait Prover {
  val bind: Map[WhatWant, Compound]
  val compound: Compound
  def is_proved: Boolean = bind.forall(item => item._2 != null)
  implicit def optional : List[Proved] = if (is_proved) List(Proved(bind, compound)) else List()
  def <<(prover: Prover): Proving = {
    val m = mutable.Map.newBuilder[WhatWant, Compound | Null].addAll(bind).result
    prover.bind.foreach((key, value) => {
      m.put(key,
        if (m.contains(key))
          if (m(key) == value) value
          else null
        else value
      )
    })

    Proving(m.toMap, compound -- prover.compound)
  }

  override def toString: String =
    compound.toString + "\t" +
      bind.toList.map(
        (what, value) => s"${what.toSimpleString}=$value"
      ).mkString("\t")

  implicit def toSolver: Solver
}

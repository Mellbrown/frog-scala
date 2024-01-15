package frog

trait Compound {
  def proveFrom(facts: frog.Facts): frog.Facts
  implicit def proved : Proved
}

package frog

trait Compound extends Goal {
  implicit def proved: Proved = Proved(Map(), this)
  
}

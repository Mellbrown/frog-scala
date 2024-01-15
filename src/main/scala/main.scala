import frog.*

import scala.language.postfixOps
@main
def main(): Unit = {
  val fact =
    <+>
     + "Person"("A")
     + "Person"("B")
     + "Person"("C")
     + "Friend"("A", "B")
     + "Friend"("B", "C")
     + "Friend"("A", "A")
    <+>? "Friend"("who"?, "who"?)

  println(fact)
}

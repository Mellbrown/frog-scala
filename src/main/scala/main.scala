import frog.*

import scala.language.postfixOps
@main
def main(): Unit = {
  val fact =
    <+>
     + "Person"("A")
     + "Person"("B")
     + "Person"("C")
     + "Cat"("D")
     + "Cat"("E")
     + "Friend"("A", "D")
     + "Friend"("B", "E")
     + "Friend"("A", "B")
     + "Friend"("B", "C")
    <+>? ("Friend"("A"?, "B"?) && "Cat"("B"?))
//    <+>? ("Friend"("A"?, "B"?) && "Cat"("B"?))

  println(fact)

  println("Friend"("A"?, "B"?) && "Cat"("B"?))
}

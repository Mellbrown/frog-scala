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
    <+>? ("Friend"("A"?, "B"?) && "C"?("B"?))
//    <+>? ("Friend"("A"?, "B"?) && "Cat"("B"?))

  println(fact)

  println("Friend"("A"?, "B"?) && "What"?("B"?))
}

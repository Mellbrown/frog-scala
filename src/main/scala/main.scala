
import frog.*
import scala.language.{implicitConversions, postfixOps}


@main
def main(): Unit = {

  println(
    <<
      - "Person"("A")
      - "Person"("B")
      - "Friend"("A", "B")
      - "Friend"("A", "C")
      - "Friend"("B", "D")
      - "Friend"("C", *)
    >> Facts()
    solve "Friend"("C", *).?
  )

  val t = "together"("Person"(*), "Person"("B")).?
  println(t)

}

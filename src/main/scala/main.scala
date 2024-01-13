
import frog.*
import scala.language.{implicitConversions, postfixOps}


@main
def main(): Unit = {

  val facts =
    <<
      - "Person"("A")
      - "Person"("B")
      - "Friend"("A", "B")
      - "Friend"("A", "C")
      - "Friend"("B", "D")
      - "Friend"("C", *)
    >> Facts()

//  println(VariableResult(List(Map("A"->$("B")))))
  val result: VariableResult = VariablePicker(facts, "Friend"($("A"), $("B"))?).pick
  println(result)

//  val t = "Relate"("A", "B") && "Relate"("B", "C") ?;
//  println(t)

}

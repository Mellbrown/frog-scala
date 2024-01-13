import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import scala.language.{implicitConversions, postfixOps}

package object frog {
  type Atomic = String | Int | Double | Boolean
  type Compound = Atom | Term | Anything | Question with Questionable
  implicit def Atomics2Compounds(lst: List[Atomic | Compound]): List[Compound] = {
    lst.map {
      case a: Atomic => new Atom(a)
      case c: Compound => c
    }
  }
  trait Questionable {
    def ? : Question
  }
  implicit class Atom(val atom: Atomic) extends Questionable {
    override def ? : Question = Question(this)

    override def equals(o: Any): Boolean = {
      o match
        case o0: Atom => atom.equals(o0.atom)
        case _ => super.equals(o)
    }
    override def toString: String = atom.toString
  }

  implicit class Predicate(val predicate: String) {
    def apply (compounds: Compound*) : Term = Term(this, compounds: _*)

    override def equals(o: Any): Boolean = {
      o match
        case o0: Predicate => predicate.equals(o0.predicate)
        case _ => super.equals(o)
    }
    override def toString: String = predicate
  }

  class Term (
    val predicate: Predicate,
    val compounds: Compound*
  ) extends Questionable {

    override def ? : Question = Question(predicate(compounds.map { compound => compound.? }*))
    override def toString: String = s"$predicate(${compounds.mkString(", ")})"
  }

  class Anything extends Questionable {
    override def ? : Question = Question(this)

    override def toString: String = "*"
  }

  class Question (
    var bind: Compound = Anything()
  ) extends Questionable {

    override def ? : Question = this
    override def toString: String = s"$bind?"
  }

  @targetName("*")
  def * : Anything = Anything()

  @targetName("?")
  def ? : Question = Question()

  implicit class Facts(val facts: List[Compound] = List()) {
    def solve (question: Question): Facts = Solver(this, question) solve
    override def toString: String = "Facts:\n - " + facts.mkString("\n - ")
  }

  @targetName("<<")
  def <<[T]: Listing[T] = Listing()
  implicit class Listing[T](val lst: ListBuffer[T] = ListBuffer[T]()) {
    @targetName("-")
    def -(o: T): Listing[T] = {
      lst.append(o)
      this
    }

    @targetName("*")
    def *(l: List[T]): Listing[T] = {
      lst.appendAll(l)
      this
    }
    @targetName(">>")
    def >>[R](singleton: R): R = {
      val clazzName = singleton.getClass.getName.stripSuffix("$")
      val clazz = Class.forName(clazzName)
      val constructor = clazz.getConstructor(classOf[List[T]])
      constructor.newInstance(lst.toList).asInstanceOf[R]
    }

  }

  class Solver (
    val facts: Facts,
    val question: Question
  ) {
    def solve :Facts = {
      val explicit_facts = Atomics2Compounds(facts.facts)
      question.bind match
        case atom0: Atom => explicit_facts.filter(
          fact => fact match
            case atom1: Atom => {
              atom0 == atom1
            }
            case _ => false
        )
        case term0: Term => explicit_facts.filter(
          fact => fact match
            case term1: Term => {
                term0.predicate == term1.predicate &&
                term0.compounds.length == term1.compounds.length &&
                term0.compounds
                    .zipWithIndex
                    .foldLeft(true) {
                      case (isTrue, (compound, index)) =>
                        isTrue && Facts(List(term1.compounds(index))).solve(compound ?).facts.nonEmpty
                    }

            }
            case _ => false
        )
        case anything: Anything => explicit_facts
        case question: Question => explicit_facts.solve(question)
        case _ => Facts()
    }
  }
}

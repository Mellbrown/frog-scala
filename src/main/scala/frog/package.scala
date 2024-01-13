import scala.annotation.targetName
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.{implicitConversions, postfixOps}

package object frog {
  type Atomic = String | Int | Double | Boolean
  type Compound = Atom | Term | Anything | Question | ShareVariable with Questionable
  type Goal = Question | AndQuestions

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

    def && (term: Term): AndTerms = AndTerms(List(this, term))
    override def ? : Question = Question(predicate(compounds.map { compound => compound.? }*))
    override def toString: String = s"$predicate(${compounds.mkString(", ")})"
  }

  class AndTerms(val andTerms: List[Term]) {
    def && (term: Term): AndTerms = AndTerms(andTerms :+ term)

    def ? : AndQuestions = new AndQuestions(andTerms map(term => term?))
    override def toString: String = andTerms.mkString(", ")
  }
  class Anything extends Questionable {
    override def ? : Question = Question(this)

    override def toString: String = "*"
  }


  class ShareVariable(val name: String) extends Questionable {

    override def toString: String = s"$$$name"
    override def ? : Question = Question(this)
  }

  def $ (name: String) : ShareVariable = new ShareVariable(name)

  class Question (
    var bind: Compound = Anything()
  ) extends Questionable {

    def && (question: Question): AndQuestions = AndQuestions(List(this, question))
    override def ? : Question = this
    override def toString: String = s"$bind?"
  }

  @targetName("*")
  def * : Anything = Anything()

  @targetName("?")
  def ? : Question = Question()

  implicit class Facts(val facts: List[Compound] = List()) {
    def solve (goal: Goal): Facts = {
      goal match
        case question: Question => QuestionSolver(this, question) solve
        case andQuestions: AndQuestions => AndQuestionSolver(this, andQuestions) solve
    }
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

  class AndQuestions(val andQuestions: List[Question]) {
    def && (question: Question): AndQuestions = AndQuestions(andQuestions :+ question)
    def ? : AndQuestions = this
    override def toString: String = andQuestions.mkString(", ")
  }

  class QuestionSolver(
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
        case shareVariable: ShareVariable => explicit_facts
    }
  }

  implicit class VariableResult(val results: List[Map[String, Compound]]) {

    override def toString: String = {
      results.head.keys.map(o => s"$$$o").mkString("\t") + "\n" +
      results.map(map0 => map0.toList.map((key, value) => value).mkString("\t")).mkString("\n")
    }
  }
  class VariablePicker(
    val facts: Facts,
    val question: Question
  ) {
    def pick : List[Map[String, Compound]] = {
      question.bind match
        case shareVariable: ShareVariable =>
          QuestionSolver(facts, * ?).solve.facts.map(
            compound => Map(shareVariable.name -> compound)
          )
        case question: Question => VariablePicker(facts, question) pick
        case anything: Anything => List()
        case term: Term =>
          QuestionSolver(facts, Term(
            term.predicate,
            term.compounds.map {
              case _: ShareVariable => *
              case compound => compound
            }*
          ) ? ).solve.facts
            .filter { o => o.isInstanceOf[Term] }
            .map {
              case term0: Term => term.compounds
                .zipWithIndex.map((compound, index) => (compound, term0.compounds(index)))
                .filter((compound0, compound1) => {
                  compound0 match
                    case shareVariable: ShareVariable => true
                    case question: Question => question.bind.isInstanceOf[ShareVariable]
                    case _ => false
                })
                .foldLeft(mutable.Map[String, Compound | Null]())((map0, pair) => {
                  val share = pair._1 match {
                    case shareVariable: ShareVariable => shareVariable
                    case question: Question => question.bind
                    case compound => compound
                  }
                  val compound = pair._2
                  share match
                    case share: ShareVariable =>
                      if (map0.contains(share.name)) {
                        if (map0.get(share.name) != compound) {
                          map0.put(share.name, null)
                        }
                      } else {
                        map0.put(share.name, compound)
                      }
                    case _ =>
                  map0
                }).filter { case (_, value) => value != null }.toMap

              case _ => Map()
            }.filter(o => o.nonEmpty)

        case atom: Atom => List()
    }
  }

  class AndQuestionSolver(
     val facts: Facts,
     val andQuestions: AndQuestions
  ) {
    def solve :Facts = {


      facts
    }
  }
}

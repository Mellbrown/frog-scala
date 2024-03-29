
import scala.language.implicitConversions

package object frog {

  def <+> : List[Proved] = List()
  implicit class StringExtension(val string: String) {
    def ? : What = What(string)

    def apply(compounds: Compound*): Term = Term(Predicate(string), compounds *)

    def ~* : frog.Predicate = Predicate(string)
  }
  implicit def oneToMany[T](one:T): List[T] = List(one)
  implicit def provedToFacts(proved: Proved): Facts = Facts(proved)
  implicit def compoundToFacts(compound: Compound): Facts = compound.proved
  implicit def FactsToSolutions(facts: Facts): Solutions = facts.facts.map(fact => fact.toSolver)
//  implicit def compoundToFact(compound: Compound): Facts = Facts(compound)
//  implicit def mutableList[T](immutableList: List[T]): mutable.ListBuffer[T] = mutable.ListBuffer[T](immutableList*)
//  implicit def immutableList[T](mutableList: mutable.ListBuffer[T]): List[T] = mutableList.toList
//  implicit def mutableMap[K, V](immutableMap: Map[K, V]): mutable.Map[K, V] = mutable.Map[K, V](immutableMap.toList*)
//  implicit def immutableMap[K, V](mutableMap: mutable.Map[K, V]): Map[K, V] = mutableMap.toMap

}

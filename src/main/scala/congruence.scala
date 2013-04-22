package my.congruence

import scala.collection.immutable.TreeMap
import scala.collection.immutable.Map
import scala.collection.immutable.TreeSet
import scala.math.Ordering
import scala.annotation._

import my.uf._
import my.ergo._

// Every step of the inference rule produces a context
class Context[T<:Rep[T]] (val use:Map[T, TreeSet[HashedTerm]], val uf: UF[T]) {
  
  private def matchArg(t1:HashedTerm, t2:HashedTerm)(implicit fac:Factory[T]) = 
    uf.areEqual(t1, t2) 
  
  private def matchList(l1:List[HashedTerm], 
    l2:List[HashedTerm]) (implicit fac:Factory[T]) : Boolean = {
    (l1 zip l2).forall({ case(x,y) => matchArg(x,y) })
  }
  
  // Decide whether two terms are known to be congruent
  def congruent(u1:HashedTerm, u2:HashedTerm) (implicit fac:Factory[T]):Boolean = {
    // Compare the symbol of the two terms
    if (u1.t.f == u2.t.f) 
      matchList(u1.t.xs, u2.t.xs)
    else false
  }

  def addTerm(t:HashedTerm): Context[T] = {
    if (uf.contains(t)) this 
    else {
      // Add the term to the context
      this
    } 
  }

}

object Context {
  import HashedTermOrdering._
  
  def empty[T<:Rep[T]] (fac:Factory[T]) = 
    new Context( Map[T,TreeSet[HashedTerm]](),UF.empty[T](fac))
  
}

package my.congruence

import scala.collection.immutable.TreeMap
import scala.collection.immutable.Map
import scala.collection.immutable.TreeSet
import scala.math.Ordering
import scala.annotation._

import my.uf._
import my.ergo._

// Every step of the inference rule produces a context
class Context[T<:Rep[T]] (
  val use:Map[T,TreeSet[HashedTerm]], 
  val uf:UF[T]) {
  
  private def matchArg(t1:HashedTerm, t2:HashedTerm)(implicit fac:Factory[T]) = 
    uf.areEqual(t1,t2) || uf.areEqual(t2, t1)
  
  
  private def matchList(f:Symbol, 
    l1:List[HashedTerm], l2:List[HashedTerm]) (implicit fac:Factory[T]) = {
    (l1 zip l2).forall( (x) => matchArg(x._1,x._2) )
  }
  
  def congruent(u1:Term, u2:Term):Boolean = {
   false 
  }
}

object Context {
  import HashedTermOrdering._
  
  def empty[T<:Rep[T]] (fac:Factory[T]) = 
    new Context( Map[T,TreeSet[HashedTerm]](),UF.empty[T](fac))
  


}

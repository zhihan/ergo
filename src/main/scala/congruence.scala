package my.congruence

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.math.Ordering
import scala.annotation._

import my.uf._
import my.ergo._

// Every step of the inference rule produces a context
class Context[T] (val use:TreeMap[T,TreeSet[T]], 
  val uf:UnionFind[T], val ord:Ordering[T]) {
  
  private def matchArg(t1:T, t2:T):Boolean = 
    uf.areEqual(t1,t2) || uf.areEqual(t2, t1)
  

  private def matchCommutative(p1:(T,T), p2:(T,T)): Boolean = 
    (uf.areEqual(p1._1, p2._1) && uf.areEqual(p1._2, p2._2)) ||
     (uf.areEqual(p1._1, p2._2) && uf.areEqual(p1._2, p2._1)) 
  
  private def matchList(f:Symbol, l1:List[T], l2:List[T]) = {
    // Special handling for commutative operators(XXX) 
    (l1 zip l2).forall( (x) => matchArg(x._1,x._2) )
  }
  
  def congruent(u1:Term, u2:Term):Boolean = {
   false 
  }
  
}

object Context {
  def empty[T] (ord:Ordering[T]) = new Context[T]( TreeMap[T,TreeSet[T]]()(ord),
    UnionFind.empty[T](ord), ord)
  
}

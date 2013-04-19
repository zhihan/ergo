package my.ergo

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.collection.immutable.Set
import scala.collection.immutable.Map

import scala.math.Ordering

import my.polynomial._

class UF[T] (val map:TreeMap[HashedTerm, List[T]], 
  val minv: Map[T,TreeSet[HashedTerm]],
  val mapm: TreeMap[HashedTerm, TreeSet[HashedTerm]], 
  val neqs: Map[T, TreeSet[HashedTerm]]) {
// map : term -> the representative
// minv: representative -> class of terms
// mapm: leaf -> set of terms whose reprentative uses this leaf
// neqs: representative -> nonequal terms    

  def rep(x:HashedTerm):T = map(x).head

}


object UF { 

  implicit object termOrder extends Ordering[HashedTerm] {
    override def compare(a:HashedTerm, b:HashedTerm) = a.hash - b.hash
  }
  

  // Update mapm map
  def addMapm[T <: Rep[T]](t:HashedTerm, r:T, 
      mapm: TreeMap[HashedTerm,TreeSet[HashedTerm]]) = {
    val leaves = r.leaves
    leaves.foldLeft(mapm) { (acc,x) => {
        val s = acc.getOrElse(x, TreeSet[HashedTerm]())
        acc + (x -> (s + t)) 
      }
    }
  }
  
  // Update minv map
  def addMinv[T <: Rep[T]](t:HashedTerm, r: T, minv: Map[T, TreeSet[HashedTerm]]) = 
    minv + (r -> (if (minv.contains(r)) minv(r) + t  else TreeSet(t) ))
  
  // Update map
  def addMap[T](t: HashedTerm, r:T, m:TreeMap[HashedTerm,List[T]]) = 
    m + (t -> (r :: m(t) ))

  // Update the nonequality map
  def addNeq[T <: Rep[T]](s: TreeSet[HashedTerm], r:T, uf:UF[T])= {
    // Check inconsistence: 
    if (s.exists( (t:HashedTerm) => 
      r.equal(uf.map(t).head))) { 
      throw new RuntimeException("Inconsistent") 
    }
    // Add to the map
    val newVal = uf.neqs.getOrElse(r, TreeSet[HashedTerm]()) ++ s
    uf.neqs + (r -> newVal)
  }


  def empty[T] = new UF( TreeMap[HashedTerm, List[T]](),
    Map[T, TreeSet[HashedTerm]] (),
    TreeMap[HashedTerm, TreeSet[HashedTerm]] (),
    Map[T, TreeSet[HashedTerm]] ())
   
}

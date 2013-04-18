package my.ergo

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.collection.immutable.Set
import scala.collection.immutable.Map

import scala.math.Ordering

import my.polynomial._

class UF[T] (val map:TreeMap[HashedTerm, List[Rep[T]]], 
  val minv: Map[Rep[T],TreeSet[HashedTerm]],
  val mapm: TreeMap[HashedTerm, TreeSet[HashedTerm]], 
  val neqs: Map[Rep[T], TreeSet[HashedTerm]]) {
// map : term -> the representative
// minv: representative -> class of terms
// mapm: leaf -> set of terms whose reprentative uses this leaf
// neqs: representative -> nonequal terms    



}


object UF { 

  implicit object termOrder extends Ordering[HashedTerm] {
    override def compare(a:HashedTerm, b:HashedTerm) = a.hash - b.hash
  }

  def addMapm[T](t:HashedTerm, r:Rep[T], mapm: TreeMap[HashedTerm,TreeSet[HashedTerm]]) = {
    val leaves = r.leaves
    leaves.foldLeft(mapm) { (acc,x) => {
        val s = acc.getOrElse(x, TreeSet[HashedTerm]())
        acc + (x -> (s + t)) 
      }
    }
  } 

  def empty[T] = new UF( TreeMap[HashedTerm, List[Rep[T]]](),
    Map[Rep[T], TreeSet[HashedTerm]] (),
    TreeMap[HashedTerm, TreeSet[HashedTerm]] (),
    Map[Rep[T], TreeSet[HashedTerm]] ())
   
}

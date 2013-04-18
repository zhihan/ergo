package my.ergo

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.math.Ordering

class UF[T] (val map:TreeMap[HashedTerm, List[Rep[T]]], 
  val minv: Map[Rep[T],TreeSet[HashedTerm]],
  val mapm: TreeMap[HashedTerm, Set[Rep[T]]], 
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


  def empty[T] = new UF( TreeMap[HashedTerm, List[Rep[T]]](),
    Map[Rep[T], TreeSet[HashedTerm]] (),
    TreeMap[HashedTerm, Set[Rep[T]]] (),
    Map[Rep[T], TreeSet[HashedTerm]] ())
   
}

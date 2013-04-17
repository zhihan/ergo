package my.ergo

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.math.Ordering

class UF (val map:TreeMap[HashedTerm, List[Rep]], 
  val minv: TreeMap[Rep,TreeSet[HashedTerm]],
  val mapm: TreeMap[HashedTerm, TreeSet[Rep]], 
  val neqs: TreeMap[Rep, TreeSet[HashedTerm]]) {
// map : term -> the representative
// minv: representative -> class of terms
// mapm: leaf -> set of terms whose reprentative uses this leaf
// neqs: representative -> nonequal terms    


}


object UF {

  implicit object termOrder extends Ordering[HashedTerm] {
    override def compare(a:HashedTerm, b:HashedTerm) = a.hash - b.hash
  }

  implicit object repOrder extends Ordering[Rep] {
    override def compare(a:Rep, b:Rep) = a.hash - b.hash
  }

  def empty = new UF( TreeMap[HashedTerm, List[Rep]](),
    TreeMap[Rep, TreeSet[HashedTerm]] (),
    TreeMap[HashedTerm, TreeSet[Rep]] (),
    TreeMap[Rep, TreeSet[HashedTerm]] ())
   
}

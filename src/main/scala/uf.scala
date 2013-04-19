package my.ergo

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.collection.immutable.Set
import scala.collection.immutable.Map

import scala.math.Ordering

import my.polynomial._

class UF[T <: Rep[T]] (val map:TreeMap[HashedTerm, List[T]], 
  val minv: Map[T,TreeSet[HashedTerm]],
  val mapm: TreeMap[HashedTerm, TreeSet[HashedTerm]],
  val neqs: Map[T, TreeSet[HashedTerm]]) {
// map : term -> the representative
// minv: representative -> class of terms
// mapm: leaf -> set of terms whose reprentative uses this leaf
// neqs: representative -> nonequal terms    
  private def mapToString = "Map:{\n" +
    map.toList.map{ 
      case(term, lr) => term.t.toString + " -> " + 
        lr.head.toString 
    }.mkString(";\n") + 
    "\n}\n"
    
  private def minvToString = "Minv:{\n" +
    minv.toList.map{
      case(rep, setT) => rep.toString + " -> " + "[" +
        setT.toList.map{
          v => v.t.toString  
        }.mkString(";") + "]"
    }.mkString(";\n") +
    "\n}\n"

  private def mapmToString = "MapM:{\n" +
    mapm.toList.map {
      case(term, setT) => term.t.toString + " -> " + "[" +
        setT.toList.map{
          v => v.t.toString  
        }.mkString(";") + "]"
    }.mkString(";\n") +
    "\n}\n"
    
  private def neqsToString = "Neqs:{\n" +
    neqs.toList.map {
      case(rep, setT) => rep.toString + " -> " + "[" +
        setT.toList.map{
          v => v.t.toString  
        }.mkString(";") + "]"
    }.mkString(";\n") +
    "\n}\n"
    

  def print = println(mapToString + minvToString +
    mapmToString + neqsToString )

  def rep(x:HashedTerm):T = map(x).head

  // Cannonitize a representative
  // If r contains a leave that has a representative, substitute
  // it with its representative
  private def canon(r:T) (fac:Factory[T]): T = {
    r.leaves.foldLeft(r)( (acc, leaf) => 
      if (map.contains(leaf)) 
        acc.subst(leaf, rep(leaf)) 
      else 
        acc.subst(leaf, fac.make(leaf))
    ) 
  }


  def add(t:HashedTerm) (fac:Factory[T]) = {
    if (map.contains(t)) 
      this
    else {
      val r = canon(fac.make(t))(fac)
      new UF( UF.addMap(t,r, map), 
        UF.addMinv(t,r,minv),
        UF.addMapm(t,r,mapm),
        UF.initNeqs(r, neqs))
    }
  }
  
  def contains(t:HashedTerm) = map.contains(t)
  
  // Get the representative, create a fresh one if not found
  // The difference from add is that we do not adding the rep
  def getR(t:HashedTerm)(fac:Factory[T]) = 
    map.getOrElse(t, fac.make(t))
    
  def classOf(t:HashedTerm) = minv(rep(t))   

  def update(x:HashedTerm, r:T) = {
    val toUpdate = mapm(x)
    toUpdate.foldLeft(this) ((acc, term) => {
      // new binding is term => R.subst(x,r)
      val oldR = rep(term)
      val newR = oldR.subst(x, r)
      val oldNeqs = neqs(oldR) 
      new UF[T]( UF.addMap(term, newR, acc.map), 
        UF.addMinv(term, newR, acc.minv),
        UF.addMapm(term, newR, acc.mapm),
        UF.addNeq(oldNeqs, newR, acc))
    })
  }

}


object UF { 

  implicit object termOrder extends Ordering[HashedTerm] {
    override def compare(a:HashedTerm, b:HashedTerm) = a.hash - b.hash
  }
  
// Update mapm map
  private def addMapm[T <: Rep[T]](t:HashedTerm, r:T, 
      mapm: TreeMap[HashedTerm,TreeSet[HashedTerm]]) = {
    val leaves = r.leaves
    leaves.foldLeft(mapm) { (acc,x) => {
        val s = acc.getOrElse(x, TreeSet[HashedTerm]())
        acc + (x -> (s + t)) 
      }
    }
  }
  
  // Update minv map
  private def addMinv[T <: Rep[T]](t:HashedTerm, r: T, minv: Map[T, TreeSet[HashedTerm]]) = 
    minv + (r -> (if (minv.contains(r)) minv(r) + t  else TreeSet(t) ))
  
  // Update map
  private def addMap[T](t: HashedTerm, r:T, m:TreeMap[HashedTerm,List[T]]) = 
    m + (t -> (r :: m.getOrElse(t, Nil) ))

  // Update the nonequality map
  private def addNeq[T <: Rep[T]](s: TreeSet[HashedTerm], r:T, uf:UF[T])= {
    // Check inconsistence: 
    if (s.exists( (t:HashedTerm) => 
      r.equal(uf.map(t).head))) { 
      throw new RuntimeException("Inconsistent") 
    }
    // Add to the map
    val newVal = uf.neqs.getOrElse(r, TreeSet[HashedTerm]()) ++ s
    uf.neqs + (r -> newVal)
  }



  def empty[T<: Rep[T]] (uf:Factory[T]) = new UF( 
    TreeMap[HashedTerm, List[T]](),
    Map[T, TreeSet[HashedTerm]] (),
    TreeMap[HashedTerm, TreeSet[HashedTerm]] (),
    Map[T, TreeSet[HashedTerm]] ())

  def initNeqs[T <: Rep[T]](r: T, neqs: Map[T,TreeSet[HashedTerm]]) = 
    if (neqs.contains(r)) 
      neqs
    else
      neqs + (r -> TreeSet[HashedTerm]())
  
}

package my.uf
// Union find

// Note: Scala has an Ordered trait in scala.math package. Here I am 
// using a similar approach. May want to replace it with Ordered trait

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.math.Ordering

// Disjoint-Set with custom compare function 
class UnionFind[T] (val map:TreeMap[T,T], val proof:TreeMap[T,T], 
    val minv:TreeMap[T,TreeSet[T]], val neqs: TreeMap[T,TreeSet[T]], 
    val ord: Ordering[T]) {

  def add(elt:T) = new UnionFind[T] ( 
    map + (elt->elt),
    proof + (elt->elt),
    minv + (elt->TreeSet(elt)(ord)),
    neqs + (elt->TreeSet[T]()(ord)),
    ord)

  // Find representative of v if not found return v.
  def find(v:T) = map.get(v) match {
    case Some(r) => r
    case None => v
  }

  def findTwo(x:T, y:T) = {
    val xR = find(x)
    val yR = find(y)
    if (ord.compare(xR, yR)== 0) throw new RuntimeException("Inconsistent")
    (xR, yR)
  }


  // Find the set that contains v 
  def classOf(v:T) = minv.get(find(v)) match {
    case Some(s) => s
    case None => TreeSet(v)(ord)
  }

  ///////// Query   
  def are_distinct(x:T, y:T) = {
    val xR = find(x)
    val yR = find(y)
    val xRNeq = neqs(xR)
    val yRNeq = neqs(yR)
    xRNeq.contains(yR) || yRNeq.contains(xR)
  }

/*  def fullpath(x:T):List[T] = {
    def pathToRoot(x:T, acc:List[T]):List[T] = {
      val next = proof(x)
      
    }
  }*/
} 

// Factory method
class UFFactory[T] (val ord:Ordering[T]) {
  def empty = new UnionFind[T](TreeMap[T,T]()(ord), TreeMap[T,T]()(ord), 
    TreeMap[T,TreeSet[T]]()(ord), TreeMap[T,TreeSet[T]]()(ord), ord)
  
  // Update for each v redirect the representative to r 
  private def updateMap( oldMap:TreeMap[T,T], r:T, v:TreeSet[T]) = 
    v.foldLeft  (oldMap)  ( (m, elem) => m + (elem -> r))  

  // Update inverse map 
  private def updateMinv(minv:TreeMap[T,TreeSet[T]], deleteR:T, keepR:T, unionV: TreeSet[T]) =
    (minv - deleteR) + (keepR -> unionV)
 
  // Update the set of non-equalifies 
  private def updateNeqs( neqs:TreeMap[T,TreeSet[T]], delR:T, keepR:T, 
    delNEq:TreeSet[T], keepNEq: TreeSet[T]) = 
    (delNEq.foldLeft (neqs) ( (m, x) => m + (x -> (neqs(x) + keepR)) )) + 
      (keepR -> (keepNEq ++ delNEq)) 
  
  // Update the proof chain
  private def updateProof(oldProof:TreeMap[T,T], x:T, y:T) : TreeMap[T,T] = {
    val next = oldProof(x) 
    val newProof = oldProof + (x -> y)
    if (ord.compare(x, next) == 0) 
      newProof
    else
      updateProof(newProof, next, x) 
  }
  
  def union( m:UnionFind[T], x:T, y:T) = {
    val xR = m.find(x)
    val yR = m.find(y)
    val xRNEq = m.neqs(xR)
    val yRNEq = m.neqs(yR)
    if (!xRNEq.contains(xR) || !yRNEq.contains(yR) ) {
      throw new RuntimeException("Inconsistence")
    }
    if (ord.compare(xR, yR) != 0 ) {
      val xS = m.minv(xR)
      val yS = m.minv(yR)
      val newS = xS ++ yS
      
      if (xS.size < yS.size) {
        // Keep yR, delete xR
        val newMap = updateMap(m.map, yR, xS)
        val newMinv = updateMinv(m.minv, xR, yR, newS)
        val newProof = updateProof(m.proof, x, y)
        val newNEqs = updateNeqs(m.neqs, xR, yR, xRNEq, yRNEq)
        (new UnionFind[T](newMap, newProof, newMinv, newNEqs, ord), 
        Some(xR))
      } else {
        // Keep xR, delete yR
        val newMap = updateMap(m.map, xR, yS)
        val newMinv = updateMinv(m.minv, yR, xR, newS)
        val newProof = updateProof(m.proof, y, x)
        val newNEqs = updateNeqs(m.neqs, yR, xR, yRNEq, xRNEq)
        (new UnionFind[T](newMap, newProof, newMinv, newNEqs, ord), 
        Some(yR))
      }
    } else
      (m, None)  // (UF, deletedR, deleted)
  }
  
  def distinct(m:UnionFind[T], x:T, y:T) = {
    val (xR, yR) = m.findTwo(x, y)
    val xRNEq = m.neqs(xR)
    val yRNEq = m.neqs(yR)
    val newNEqs =  m.neqs + (yR -> (yRNEq + xR)) + (xR->(xRNEq + yR))
    new UnionFind[T](m.map, m.proof, m.minv, newNEqs, ord)
  }  
 
}

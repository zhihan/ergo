package my.uf
// Union find

// Note: Scala has an Ordered trait in scala.math package. Here I am 
// using a similar approach. May want to replace it with Ordered trait

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.math.Ordering
import scala.annotation._

abstract class Formula
case class Eq[T](val lhs:T, val rhs:T) extends Formula
case class Neq[T](val lhs:T, val rhs:T) extends Formula

object Formula {
  def makeEqualities[T](l:List[T]) : List[Eq[T]] = {
     // Recursive function
    def loop(l:List[T], top:T, acc:List[Eq[T]]): List[Eq[T]] = {
      l match {
        case Nil => acc
        case hd :: tail => loop (tail, hd, Eq[T](top, hd) :: acc)
      } 
    }
    l match {
      case Nil | _ ::Nil => Nil
      case hd :: tail => loop(tail, hd, List[Eq[T]]())
    } 
  }
}

class PrintUtil[T] {
  def toS(f:Formula) :String = {
    f match {
      case Eq(lhs, rhs) => lhs.toString + "=" + rhs.toString
      case Neq(lhs, rhs) => lhs.toString + "!=" + rhs.toString
    }
  }
  def printList(l:List[Formula]) {
    l.foreach ( e => { 
      print(e.toString)
      print("\n")
    })
  }
}

// Disjoint-Set with custom compare function, instead of type-class the 
// client must provide the ordering explicitly  
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
  def areDistinct(x:T, y:T) = {
    val xR = find(x)
    val yR = find(y)
    val xRNeq = neqs(xR)
    val yRNeq = neqs(yR)
    xRNeq.contains(yR) || yRNeq.contains(xR)
  }

  def areEqual(x:T, y:T) = {
    val xR = find(x)
    val yR = find(y)
    ord.compare(xR, yR) == 0
  }

  def fullpath(x:T):List[T] = {
    // Tail-recursive subroutine
    @tailrec def pathToRoot(x:T, acc:List[T]):List[T] = {
      val next = proof(x)
      if (ord.compare(x, next) == 0) 
        acc
      else
        pathToRoot(next, next :: acc)
    }
    pathToRoot(x, List[T](x))
  }

  // Explain why x and y are equal 
  def explain(x:T, y:T): List[Eq[T]] = {
    val xR = find(x)
    val yR = find(y)
    if (ord.compare(xR, yR) == 0) {
      val listX = fullpath(x)
      val listY = fullpath(y)
      val xEqs = Formula.makeEqualities[T](listX)
      val yEqs = Formula.makeEqualities[T](listY)
      xEqs ++ yEqs   
    } else
      // They are not known to be equal
      Nil
  }

  // Explain why x and y are not equal
  def explainNeq(x:T, y:T): List[Formula] = {
    val xR = find(x)
    val yR = find(y)
    if (areDistinct(xR, yR) ) {
      val xEqs = explain(x, xR)
      val yEqs = explain(yR, y)
      Neq[T](xR,yR) :: ( xEqs ++ yEqs )
    } else
      // They are not known to be equal
      Nil
    
  }

  // Creational patterns: union and distinct
  def union2(x:T, y:T) = {
    // Update for each v redirect the representative to r 
    def updateMap( oldMap:TreeMap[T,T], r:T, v:TreeSet[T]) = 
    v.foldLeft  (oldMap)  ( (m, elem) => m + (elem -> r))  

    // Update inverse map 
    def updateMinv(minv:TreeMap[T,TreeSet[T]], deleteR:T, 
        keepR:T, unionV: TreeSet[T]) =
      (minv - deleteR) + (keepR -> unionV)
 
    // Update the set of non-equalifies 
    def updateNeqs( neqs:TreeMap[T,TreeSet[T]], delR:T, keepR:T, 
      delNEq:TreeSet[T], keepNEq: TreeSet[T]) = 
    (delNEq.foldLeft (neqs) ( (m, x) => m + (x -> (neqs(x) + keepR)) )) + 
      (keepR -> (keepNEq ++ delNEq)) 
  
    // Update the proof chain
    def updateProof(oldProof:TreeMap[T,T], x:T, y:T) : TreeMap[T,T] = {
      val next = oldProof(x) 
      val newProof = oldProof + (x -> y)
      if (ord.compare(x, next) == 0) 
        newProof
      else
        updateProof(newProof, next, x) 
    }

    val xR = find(x)
    val yR = find(y)
    val xRNEq = neqs(xR)
    val yRNEq = neqs(yR)
    if (xRNEq.contains(xR) || yRNEq.contains(yR) ) {
      throw new RuntimeException("Inconsistence")
    }
    if (ord.compare(xR, yR) != 0 ) {
      val xS = minv(xR)
      val yS = minv(yR)
      val newS = xS ++ yS
      
      if (xS.size < yS.size) {
        // Keep yR, delete xR
        val newMap = updateMap(map, yR, xS)
        val newMinv = updateMinv(minv, xR, yR, newS)
        val newProof = updateProof(proof, x, y)
        val newNEqs = updateNeqs(neqs, xR, yR, xRNEq, yRNEq)
        val r = new UnionFind[T](newMap, newProof, newMinv, newNEqs, ord)
        (r, Some(xR), Some(yR))
     } else {
        // Keep xR, delete yR
        val newMap = updateMap(map, xR, yS)
        val newMinv = updateMinv(minv, yR, xR, newS)
        val newProof = updateProof(proof, y, x)
        val newNEqs = updateNeqs(neqs, yR, xR, yRNEq, xRNEq)
        val r= new UnionFind[T](newMap, newProof, newMinv, newNEqs, ord)
        (r, Some(xR), Some(yR))
      }
    } else
      (this, None, None)
  }
  
  // Simple version of union, for testing purposes 
  def union(x:T, y:T) = {
    val(r, _, _ ) = union2(x,y)
    r
  }
  
  def distinct(x:T, y:T) = {
    val (xR, yR) = findTwo(x, y)
    val xRNEq = neqs(xR)
    val yRNEq = neqs(yR)
    val newNEqs =  neqs + (yR -> (yRNEq + xR)) + (xR->(xRNEq + yR))
    new UnionFind[T](map, proof, minv, newNEqs, ord)
  }  
 

} 

// Factory method
object UnionFind {
  def empty[T] (ord:Ordering[T]) = 
    new UnionFind[T](TreeMap[T,T]()(ord), TreeMap[T,T]()(ord), 
      TreeMap[T,TreeSet[T]]()(ord), TreeMap[T,TreeSet[T]]()(ord), ord)
  
}

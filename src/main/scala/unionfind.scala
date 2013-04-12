package my.uf
// Union find

// Note: Scala has an Ordered trait in scala.math package. Here I am 
// using a similar approach. May want to replace it with Ordered trait

import scala.collection.immutable.Map
import scala.collection.immutable.Set

trait Comp[T] {
  def compare(t1:T, t2:T): Int
}

// The union-find implementation uses typeclass pattern of Scala

// Disjoint-Set 
class UnionFind[T] (val map:Map[T,T], val proof:Map[T,T], 
    val minv:Map[T,Set[T]], val neqs: Map[T,Set[T]]) {

  def add(elt:T) = new UnionFind[T] ( 
    map + (elt->elt),
    proof + (elt->elt),
    minv + (elt->Set(elt)),
    neqs + (elt->Set[T]()))

  // Find representative of v if not found return v.
  def find(v:T) = map.get(v) match {
    case Some(r) => r
    case None => v
  }

  // Find the set that contains v 
  def classOf(v:T) = minv.get(find(v)) match {
    case Some(s) => s
    case None => Set(v)
  }
    
} 

// Factory method
class UFFactory[T] {
  def empty = new UnionFind[T](Map[T,T](), Map[T,T](), 
    Map[T,Set[T]](), Map[T,Set[T]]())
  
}

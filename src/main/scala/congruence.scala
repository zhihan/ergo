package my.congruence

import scala.collection.immutable.TreeMap
import scala.collection.immutable.Map
import scala.collection.immutable.TreeSet
import scala.math.Ordering
import scala.annotation._

import my.uf._
import my.ergo._

// Every step of the inference rule produces a context
class Context[T<:Rep[T]] (val use:Map[T, TreeSet[HashedTerm]], val uf: UF[T]) {
  import my.ergo.UF.termOrder
 
  private def matchArg(t1:HashedTerm, t2:HashedTerm)(implicit fac:Factory[T]) = 
    uf.areEqual(t1, t2) 
  
  private def matchList(l1:List[HashedTerm], 
    l2:List[HashedTerm]) (implicit fac:Factory[T]) : Boolean = {
    (l1 zip l2).forall({ case(x,y) => matchArg(x,y) })
  }
  
  // Decide whether two terms are known to be congruent
  def congruent(u1:HashedTerm, u2:HashedTerm) (implicit fac:Factory[T]):Boolean = {
    // Compare the symbol of the two terms
    if (u1.t.f == u2.t.f) 
      matchList(u1.t.xs, u2.t.xs)
    else false 
  }

  // Add a term to the CC context 
  def addTerm(t:HashedTerm)(implicit fac:Factory[T]): Context[T] = {
    if (uf.contains(t)) this 
    else {
      // 1) Add the term to the UF context
      val ctxWithT = new Context[T](use, uf.add(t))
      val xs = t.t.xs
      // Recursively add arguments 
      val ctx = xs.foldLeft(ctxWithT)((acc,x) => acc.addTerm(x))

      // 2) Update use with: "t uses leaf rep's of xs" 
      val rt = uf.find(t) 
      val useWithT = ctx.use + (rt -> TreeSet[HashedTerm]())
      val leafRepXs = xs.foldLeft(Set[T]())(
          (acc,x) => acc ++ ctx.uf.leafReps(x) )
      val newUse = leafRepXs.foldLeft(useWithT) (
        (acc, xr) => {
            val useOfL = acc(xr)
            acc + (xr -> (acc(xr) + t))
          }
        ) 
      new Context[T](newUse, ctx.uf)
    } 
  }

  // Update the context for a substitution 
  private def update(p: HashedTerm, hasP: TreeSet[HashedTerm], 
    leafReps: List[T]): Context[T] = {
         val termsUseP = acc.use(p)
       // Update use map: terms use p now uses leaf of p-value
        val newUse = leafRepsP.foldLeft (acc.use) (
          (accUse, leafR) => {
            val oldUseVal = acc.use(leafR)
            use + (leafR -> (oldUseVal ++ termsUseP) )
          }
        )  

        // Get all affected terms
        val allAffected = hasP.foldLeft (termsUseP)(
          // For each term that uses p,
          //  find the corresponding new rep, all uses of the 
          //  leaves of the new rep need to be considered for equivalence
          (acc, changedT) =>
            acc ++ newUf.leafReps(changedT).foldLeft (acc) (
            (affected, lr) => affected ++ use(lr)
          )
        )
       // Recursively add congruences  
   
  }


  // Add the fact that t1 and t2 are congruent 
  def addCongruence(t1: HashedTerm, t2:HashedTerm)
    (implicit fac:Factory[T]) : Context[T] = {
    if (uf.areEqual(t1, t2)) 
      // If t1 and t2 are known to be equal, nothing to do
      this
    else 

      val (newUf, result) = uf.union(t1, t2)
      // Reverse the substitution order
      val resultOrdered = result.reverse

      // 
      resultOrdered.foldLeft ( ) (
        val (p, hasP, leafRepsP) = x

      this 
    }
  }

}

object Context {
  import HashedTermOrdering._
  
  def empty[T<:Rep[T]] (fac:Factory[T]) = 
    new Context( Map[T,TreeSet[HashedTerm]](),UF.empty[T](fac))

}

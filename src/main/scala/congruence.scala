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
  private def useToString = "Use:{\n" +
    use.toList.map{ 
      case(rep, setT) => rep.toString + " -> " + "[" +
        setT.toList.map{
          v => v.t.toString  
        }.mkString(";") + "]"
    }.mkString(";\n") +
    "\n}\n"

  def print {
    println(useToString)
    uf.print
  }
 
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

  private def congruents(t: HashedTerm, tl:TreeSet[HashedTerm])
    (implicit fac:Factory[T]): List[(HashedTerm,HashedTerm)] = {
    tl.filter( t2 => congruent(t,t2)).map(t2 => (t,t2) ).toList
  }

  // Add a term to the CC context and update use 
  private def addATerm(t:HashedTerm)(implicit fac:Factory[T]): Context[T] = {
    if (uf.contains(t)) this 
    else {
      // 1) Add the term to the UF context
      val ctxWithT = new Context[T](use, uf.add(t))
      val xs = t.t.xs
      // Recursively add arguments 
      val ctx = xs.foldLeft(ctxWithT)((acc,x) => acc.addATerm(x))

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

  // Add a term to the CC context; and discover new congruences to 
  // facilitate incremental uses.
  def addTerm(tin:HashedTerm)(implicit fac:Factory[T]): Context[T] = {
    val allTerms = tin.subTerms
    allTerms.foldLeft (this) ( (acc, t) => {
      val ctx = acc.addATerm(t)
      val xs = t.t.xs
      // Rep's of the leaves of the arguments
      val leafRepXs = xs.foldLeft(Set[T]())(
          (ac,x) => ac ++ ctx.uf.leafReps(x) )
      val newCtx = leafRepXs.foldLeft(ctx)( (innerAcc, rx) => {
        // Terms that uses the leaf
        val leafUses = use(rx)
        leafUses.foldLeft(innerAcc) ((inAcc, leafUse) => {
          if (inAcc.congruent(t,leafUse)) 
            inAcc.addCongruence(t, leafUse)
          else inAcc
        })
      })
      newCtx
    })
  }


  // Update the context for a substitution 
  private def update(p: T, hasP: TreeSet[HashedTerm], 
    leafRepsP: List[T]) (implicit fac:Factory[T]): Context[T] = {
    val termsUseP = use(p)
    // Update use map: terms use p now uses leaf of p-value
    val newUse = leafRepsP.foldLeft (use) (
      (accUse, leafR) => {
      val oldUseVal = accUse(leafR)
      use + (leafR -> (oldUseVal ++ termsUseP) )
      }
    )  
    val newCtx = new Context[T](newUse, uf) 
    // Get all affected terms
    val allAffected = hasP.foldLeft (termsUseP)(
    // For each term that uses p,
    //  find the corresponding new rep, all uses of the 
    //  leaves of the new rep need to be considered for congruence
      (acc, changedT) =>
        acc ++ uf.leafReps(changedT).foldLeft (acc) (
          (affected, lr) => affected ++ use(lr)
        )
    )
    // Recursively add congruences
    termsUseP.foldLeft (newCtx) (
      (acc, term) => allAffected.foldLeft (acc) (
        (accInner, otherTerm) =>
          // If congruence can be determined from the term representation,
          // union the representatives in UF module.
          if (accInner.congruent(term, otherTerm))
            addCongruence(term, otherTerm)
          else accInner 
      )
    )
  }


  // Add the fact that t1 and t2 are congruent 
  def addCongruence(t1: HashedTerm, t2:HashedTerm)
    (implicit fac:Factory[T]) : Context[T] = {
    if (uf.areEqual(t1, t2)) 
      // If t1 and t2 are known to be equal, nothing to do
      this
    else { 
      val (newUf, result) = uf.union(t1, t2)
      // Reverse the substitution order
      val resultOrdered = result.reverse

      val newCtx = new Context[T](use, newUf)
      resultOrdered.foldLeft ( newCtx ) ( 
        (acc, x) => {
        val (p, hasP, leafRepsP) = x
        acc.update(p, hasP, leafRepsP)
      })
    }
  }

  def areEqual(t1: HashedTerm, t2: HashedTerm)
    (implicit fac:Factory[T]) = uf.areEqual(t1,t2)

}

object Context {
  import HashedTermOrdering._
  
  def empty[T<:Rep[T]] (fac:Factory[T]) = 
    new Context( Map[T,TreeSet[HashedTerm]](),UF.empty[T](fac))

}

package my.ergo

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.collection.immutable.Set
import scala.collection.immutable.Map

import scala.math.Ordering

import scala.annotation._

import my.polynomial._

class Inconsistent(msg:String=null, cause:Throwable=null)
  extends RuntimeException(msg, cause)
{
  val message = msg
}

class UF[T <: Rep[T]] (val map:TreeMap[HashedTerm, List[T]], 
  val minv: Map[T,TreeSet[HashedTerm]],
  val mapm: TreeMap[HashedTerm, TreeSet[HashedTerm]],
  val neqs: Map[T, TreeSet[HashedTerm]]) {

  // Define the implicit ordering of terms in this class
  import UF.termOrder


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

  def find(x:HashedTerm)(implicit fac:Factory[T]): T = 
    if (map.contains(x)) rep(x) else fac.make(x)

  // Cannonitize a representative
  // If r contains a leave that has a representative, substitute
  // it with its representative
  private def canon(r:T) (implicit fac:Factory[T]): T = {
    r.leaves.foldLeft(r)( (acc, leaf) => 
      if (map.contains(leaf)) 
        acc.subst(leaf, rep(leaf)) 
      else 
        acc.subst(leaf, fac.make(leaf))
    ) 
  }


  def add(t:HashedTerm) (implicit fac:Factory[T]) = {
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

  def leafReps(t:HashedTerm) : List[T] = {
    val r = rep(t) // Assuming rep(t) exists
    r.leaves.map( e => rep(e))
  }  

  // Update the value of a term 
  def update(x:HashedTerm, r:T): UF[T] = {
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

  // Union operation with additional return arguments:
  // (p,touched,leaves) : 
  // * p - (old) reps of the term that are solved by the solver
  // * hasP - terms that has p as its leaf
  // * leafRepsP - (new) leaf Rep's of the new rep of p.
  type Result = (T, TreeSet[HashedTerm], List[T])
  def union(x: HashedTerm, y:HashedTerm) (implicit fac:Factory[T]): 
  (UF[T], List[Result]) = {
    // Declare implicit factory for convenience
    val env = this.add(x).add(y)
    val xR = env.rep(x)
    val yR = env.rep(y)
    if (xR equal yR) 
      (env, List[Result]())
    else 
      // Check for inconsistence
      if (env.neqs(xR).exists( xN => y equal xN) || 
          env.neqs(yR).exists( yN => x equal yN)) 
        throw new Inconsistent("union")
      else {
        val (sol, yesno) = xR.solve(yR)
        yesno match {
          case No => throw new Inconsistent("union")
          case Yes => {
            sol.foldLeft(
              (env, List[Result]())) ( (acc, res) => {
              val (term, value) = res
              val (accEnv, accRes) = acc
              val p = accEnv.rep(term)
              val hasP = accEnv.mapm(term)
              val leafRepsP = value.leaves.map(t => accEnv.rep(t))
              (accEnv.update(term, value), 
                (p, hasP, leafRepsP) :: accRes )
            })
          } 
        } 
      } 
  }


  def distinct(x: HashedTerm, y:HashedTerm)(implicit fac:Factory[T]) = {
    val env = this.add(x).add(y)
    val xR = env.rep(x)
    val yR = env.rep(y)
    if (xR equal yR) throw new Inconsistent("distinct")
    
    val xN = env.neqs(xR)
    val yN = env.neqs(yR)
    
    new UF[T](env.map, env.minv, env.mapm,
      env.neqs + (xR -> (xN +y)) + (yR -> (yN+x)))
  }

  def areEqual(t1:HashedTerm, t2:HashedTerm)(implicit fac:Factory[T]) = {
    val r1 = if (map.contains(t1))
        rep(t1) 
      else 
        canon(fac.make(t1))
    val r2 = if (map.contains(t2))
        rep(t2)
      else
        canon(fac.make(t2))
    r1 equal r2
  }

  def areDistinct(t1:HashedTerm, t2:HashedTerm)= {
    if (!map.contains(t1) || !map.contains(t2))
      // If either is not found, assume false
      false
    else {
      val r1 = rep(t1)
      val r2 = rep(t2)
      val n1 = neqs(r1)
      val n2 = neqs(r2)
      n1.contains(t2) || n2.contains(t1)
    }
  }

  def explain[T<:Rep[T]] (x:HashedTerm, y:HashedTerm) = {
    val h1 = map(x)
    val h2 = map(y)
    UF.commonExpr(h1,h2)
  }

}


object UF { 
  // Implicit ordering of hashed terms 
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
  private def addMinv[T <: Rep[T]](t:HashedTerm, r: T, 
    minv: Map[T, TreeSet[HashedTerm]]) = 
    minv + (r -> (if (minv.contains(r)) minv(r) + t  else TreeSet(t) ))
  
  // Update map
  private def addMap[T](t: HashedTerm, r:T, m:TreeMap[HashedTerm,List[T]]) = 
    m + (t -> (r :: m.getOrElse(t, Nil) ))

  // Update the nonequality map
  private def addNeq[T <: Rep[T]](s: TreeSet[HashedTerm], r:T, uf:UF[T])= {
    // Check inconsistence: 
    if (s.exists( (t:HashedTerm) => 
      r.equal(uf.map(t).head))) { 
      throw new Inconsistent("AddEq") 
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

  private def commonExpr[T<:Rep[T]](h1:List[T], h2:List[T]) = {
    @tailrec def loop(x:List[T], y:List[T], acc:List[T]):List[T] = {
      (x,y) match {
        case (Nil,_) => acc
        case (_, Nil) => acc
        case (r1:: xtail, r2::ytail) => 
          if (r1 equal r2) 
            loop(xtail, ytail, r1 :: (r2 :: acc))  
          else acc
      }
    }
    loop(h1, h2, Nil)
  }
 
}

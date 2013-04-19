package my.ergo

import scala.math.Ordering
 
// A representative of a theory is an abstract form of term

// To simplify we do not assume Rep class is hash cons'ed. 
trait Rep[ConcreteRep] {
  def leaves: List[HashedTerm] 
  // Note: cannot override the equals method of 'Any'
  def equal(other: ConcreteRep) :Boolean
  def subst(x:HashedTerm, v:ConcreteRep) : ConcreteRep
  def solve(y: ConcreteRep): (List[(HashedTerm, ConcreteRep)], Solution)
     
  def hashCode : Int
  def equals(that:Any): Boolean
}

sealed abstract class Solution
object No extends Solution
object Yes extends Solution 

abstract class Factory[ConcreteRep] {
  def make(x:HashedTerm): ConcreteRep
}


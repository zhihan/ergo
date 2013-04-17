package my.ergo

import scala.math.Ordering
 
// A representative of a theory is an abstract form of term
trait Rep {
  def leaves: Set[HashedTerm] 
  def equal(other: Rep) :Boolean
  def subst(x:HashedTerm, v:Rep) :Rep
  def hash: Int
}

abstract class Theory {
  def solve(x:Rep, y:Rep): List[(HashedTerm, Rep)]
  def make(x:HashedTerm): Rep
}


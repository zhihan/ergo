package test.congruence

import my.ergo._
import my.util._
import my.congruence._

import org.scalatest.FunSuite

class StringTermRep(val t:HashedTerm) extends Rep[StringTermRep] {
  // Term represented by its toString
  override def leaves: List[HashedTerm] = List(t)
  override def equal(that:StringTermRep) = (that.toString == toString)
  override def equals(other:Any): Boolean = {
    other match {
      case that:StringTermRep => (that.toString == toString)
      case _ => false
    }
  }
  override def subst(x: HashedTerm, v:StringTermRep) = 
    if (x.toString == t.toString) v else this
  override def hashCode = t.toString.hashCode
  override def solve(y: StringTermRep) = 
    if (t.toString <= y.toString) 
      (List((t,y)), Yes)
    else 
      (List((y.t,this)), Yes)
  override def toString = t.toString
}

object StringTermFactory extends Factory[StringTermRep] {
  def make(x:HashedTerm) = new StringTermRep(x) 
}

class StringRepSuite extends FunSuite {
  test("Example from paper") {
    implicit val fac = StringTermFactory
  
    val a = HashConSymbol.make(Var("a"), SReal)
    val at = HashConTerm.make(a.sv, List())
    val b = HashConSymbol.make(Var("b"), SReal)
    val bt = HashConTerm.make(b.sv, List())
    val ctx = Context.empty[StringTermRep](StringTermFactory)
    val ctx0 = ctx.addTerm(at).addTerm(bt)
    val ctx1 = ctx0.addCongruence(at,bt) 

    val f = HashConSymbol.make(Name("f"), SReal)
    val fat = HashConTerm.make(f.sv, List(at))
    val u = HashConSymbol.make(Var("u"), SReal)
    val ut = HashConTerm.make(u.sv, List())
    val ctx2 = ctx1.addTerm(fat).addTerm(ut).addCongruence(fat,ut)

    val fbt = HashConTerm.make(f.sv, List(bt))
    val v = HashConSymbol.make(Var("v"), SReal)
    val vt = HashConTerm.make(v.sv, List())
    val ctx3 = ctx2.addTerm(fbt).addTerm(vt).addCongruence(fbt,vt)
  
    assert(ctx3.congruent(fat, fbt)) 
    assert(ctx3.areEqual(vt, ut))

 
  }
}

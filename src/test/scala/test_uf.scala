package test.uf

import my.ergo._
import my.polynomial._

import org.scalatest.FunSuite

import my.polynomial.RationalConversion._

class UFSuite extends FunSuite {
  test("Add items") {
    val x = HashConSymbol.make(Var("x"), SReal)
    val xt = HashConTerm.make(x.sv, List[HashedTerm]())
    val a = UF.empty[AffineRep](AffineRep)
    val b = a.add(xt)(AffineRep)

    assert(b.map.size == 1)
    assert(b.minv.size == 1)
    assert(b.mapm.size == 1)

    val x2 = HashConTerm.make(HashConSymbol.realPlus, List(xt,xt))
    val c = b.add(x2)(AffineRep)
    
    assert(c.map.size == 2)
    assert(c.minv.size == 2)
    assert(c.mapm.size == 1)
    assert(c.mapm(xt).size == 2)
  }
  
  test("Query items") {
    val x = HashConSymbol.make(Var("x"), SReal)
    val xt = HashConTerm.make(x.sv, List[HashedTerm]())
    val a = UF.empty[AffineRep](AffineRep)
    val b = a.add(xt)(AffineRep)
    val one = HashConSymbol.make(Const(1), SReal)
    val onet = HashConTerm.make(one.sv, List())

    val x2 = HashConTerm.make(HashConSymbol.realPlus, List(xt,onet))
    val x3 = HashConTerm.make(HashConSymbol.realPlus, List(onet,xt))
    val b1 = b.add(x2)(AffineRep)
    val c = b1.add(x3)(AffineRep)

    assert(c.classOf(x2).size == 2)
    assert(c.contains(xt) && c.contains(x2))
  } 
  
  test("Update value") {
    val x = HashConSymbol.make(Var("x"), SReal)
    val xt = HashConTerm.make(x.sv, List[HashedTerm]())
    val a = UF.empty[AffineRep](AffineRep)
    val b = a.add(xt)(AffineRep)
    val one = HashConSymbol.make(Const(1), SReal)
    val onet = HashConTerm.make(one.sv, List())

    val x2 = HashConTerm.make(HashConSymbol.realPlus, List(xt,onet))
    val c = b.add(x2)(AffineRep)
    val c1 = c.add(onet)(AffineRep)
    val d = c1.update(xt, c1.rep(onet))

    assert(d.rep(x2).f.isValue && d.rep(x2).f.b == Rational(2,1))

  }

  test("Union value") {
    val x = HashConSymbol.make(Var("x"), SReal)
    val xt = HashConTerm.make(x.sv, List[HashedTerm]())
    val a = UF.empty[AffineRep](AffineRep)
    val b = a.add(xt)(AffineRep)
    val one = HashConSymbol.make(Const(1), SReal)
    val onet = HashConTerm.make(one.sv, List())
    
    val x2 = HashConTerm.make(HashConSymbol.realPlus, List(xt,onet))
    val c = b.add(x2)(AffineRep)
    val d = c.union(xt, onet)(AffineRep)

    d.print
    assert(d.rep(x2).f.isValue && d.rep(x2).f.b == Rational(2,1))

  }

   
 
  
}

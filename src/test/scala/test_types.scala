package test.types

import my.ergo._
import my.util._

import org.scalatest.FunSuite

class Suite extends FunSuite {
  test("Symbol views") {
    HashConSymbol.clear
    val v1 = HashConSymbol.make(Name("f"), SReal)
    val v2 = HashConSymbol.make(Name("f"), SReal)
    assert(v1.tag == v2.tag)

    val v3 = HashConSymbol.make(Name("f"), SInt)
    assert(v1.tag != v3.tag)
  }

  test("Terms") {
    HashConSymbol.clear
    HashConTerm.clear
    val f = HashConSymbol.make(Name("f"), SReal)
    val x = HashConSymbol.make(Var("x"), SReal)
    val xt = HashConTerm.make(x.sv, List())
    val t1 = HashConTerm.make(f.sv, List(xt))
    val t2 = HashConTerm.make(f.sv, List(xt))
    assert(t1.tag == t2.tag)
  }

  test("Term substitution") {
    HashConSymbol.clear
    HashConTerm.clear
    val f = HashConSymbol.make(Name("f"), SReal)
    val x = HashConSymbol.make(Var("x"), SReal)
    val xt = HashConTerm.make(x.sv, List())

    val c = HashConSymbol.make(Const(1), SReal)
    val ct = HashConTerm.make(c.sv, List())
    val m = Map[SymbolView,HashedTerm](x.sv -> ct)

    val t = HashConTerm.make(f.sv, List(xt))
  
    val result = HashConTerm.make(f.sv, List(ct))
    assert(result.equal(t.subst(m)))
  }
}

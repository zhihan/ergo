package test.polynomial

import my.ergo._
import my.util._
import my.polynomial._

import org.scalatest.FunSuite
import my.polynomial.RationalConversion._


class PolynomialSuite extends FunSuite {
  test("Rational computing") {
    val zero = Rational(0,2)
    val zero2 = Rational(0,1)
    assert(zero == zero2)

    assert(Rational(1,2) + 1 == Rational(3,2))
    assert(Rational(1,2).uminus == Rational(-1,2))
    assert(Rational(1,2) * 2 == 1)
    assert(Rational(1,2) / 2 == Rational(1,4))
    assert(Rational(1,2) - Rational(1,2) == 0)
    assert(Rational(0,3).isZero)
  }

  test("Affine forms") {
    val x = V("x")
    val fx = Affine(Map(x -> Rational(1,1)), 0)
    assert(fx.A(x) == Rational(1,1))
    assert(fx.b == 0)
    
    val k = Affine(Map[V,Rational](), Rational(2,1))
    assert(fx * k == Affine(Map(x -> Rational(2,1)), 0))
    assert(fx * k == fx * Rational(2,1))

    assert(fx + k == Affine(Map(x -> Rational(1,1)), Rational(2,1)))
    assert(fx + k == fx + Rational(2,1))
     
    assert(fx - k == Affine(Map(x -> Rational(1,1)), Rational(-2,1)))
    assert(fx - k == fx - Rational(2,1))

    assert(fx / k == Affine(Map(x -> Rational(1,2)), Rational(0,1)))
    assert(fx / k == fx / Rational(2,1))
 
  }
  test("Solve affine equation") {
    val x = V("x")
    val fx = Affine(Map(x -> Rational(1,1)), 1)
  
    val(s, sol) = fx.solve 
    assert(sol == Yes)
    assert(!s.isEmpty)

    val (variable, value) = s.get
    assert(fx.subst(variable, value).isZero)

    val y = V("y")
    val fy = Affine(Map(y -> Rational(1,1)), 1)
    assert(fx.subst(x, fy) == Affine(Map(y -> Rational(1,1)), 2)) 
    val z = fx.subst(x, fy) - Affine(Map(y -> Rational(1,1)), 1)
    assert(z.isValue && z.b == 1)
  }

   test("Solve affine equation with variable") {
    val x = V("x")
    val y = V("y")
    val fx = Affine(Map(x -> Rational(1,1), y-> Rational(-1,1)), 0)
  
    val(s, sol) = fx.solve 
    assert(sol == Yes)
    assert(!s.isEmpty)


    val (variable, value) = s.get
    assert(fx.subst(variable, value).isZero)

  }
  
  test("Polynomial theory solver") {
    val x = HashConSymbol.make(Var("x"), SReal)
    val y = HashConSymbol.make(Var("y"), SReal)
    val xt = HashConTerm.make(x.sv, List())
    val yt = HashConTerm.make(y.sv, List())
    val one = HashConSymbol.make(Const(1), SReal)
    val onet = HashConTerm.make(one.sv, List())

    val x_p_1 = HashConTerm.make(HashConSymbol.realPlus, 
      List(xt, onet))

    val x_p_1_r = AffineRep.make(x_p_1)
    val y_r = AffineRep.make(yt)

    val (st, sol) = x_p_1_r.solve(y_r)
    assert(sol == Yes)
    assert(!st.isEmpty)

    val (term, value) = st.head
    assert(x_p_1_r.subst(term, value) equal y_r)
    
    val xleaves = x_p_1_r.leaves
    assert((xleaves.head equal xt) && xleaves.tail == Nil)

    val yleaves = y_r.leaves
    assert((yleaves.head equal yt) && yleaves.tail == Nil)
  }
  
}

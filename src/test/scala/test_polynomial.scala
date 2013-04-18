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
}

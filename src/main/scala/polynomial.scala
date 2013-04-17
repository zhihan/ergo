package my.polynomial

import scala.annotation._
import scala.math._
import scala.collection.immutable.Map

case class Rational(n:Int, d:Int)
{
  @tailrec private def gcd(x:Int, y:Int): Int = 
    if (x==0) y else gcd(y%x, x)

  private def normalize(n:Int,d:Int) = {
    val g = gcd(n,d)
    val nn = n/g
    val nd = d/g
    if (nd < 0 )
      (-nn, -nd)
    else
      (nn, nd) 
  }

  val (num,den) = normalize(n,d)
  
  override def toString= num.toString +"/" + den.toString

  def +(that:Rational) = new Rational(num * that.den + den *that.num ,den*that.den)
  def -(that:Rational) = new Rational(num * that.den - den *that.num ,den*that.den)
  def *(that:Rational) = new Rational(num * that.num, den * that.den)
  def /(that:Rational) = new Rational(num * that.den, den * that.num)
  def isZero: Boolean = (num == 0)
}

object RationalConversion {
  // Implicit conversion from integer
  implicit def fromInt(x:Int) = Rational(x, 1)
}

case class Var(val name:String) {
}

// Affine forms ax + b 
case class Affine (vars:Map[Var,Rational], val b:Rational) {
  private def normalize(s:Map[Var,Rational]):Map[Var,Rational] = {
    s.filter {case (_,a) => !a.isZero }
  }

  val A = normalize(vars)

  override def toString = A.toList.map{
    case(v,e) => e.toString + v.name.toString}.mkString("+") + 
      " + " + b.toString 

  def +(that:Affine): Affine = {
    val a = for ((v,e) <- that.A ) yield (
      if (A.contains(v)) v-> (A(v) + that.A(v)) else v-> that.A(v))
    Affine(A ++ a, b + that.b)
  }

  def +(that:Rational): Affine = Affine(A, b + that)
  
  def -(that:Rational): Affine = Affine(A, b - that)
  def -(that:Affine): Affine = this + that* Rational(-1,1)

  def *(that:Rational): Affine = Affine(A.mapValues(e => e * that), b * that)
  def *(that:Affine): Affine = if (that.A.isEmpty) 
      this*that.b 
    else 
      throw new RuntimeException("Nonlinear")
  
  def /(that:Rational):Affine = Affine(A.mapValues(e => e / that), b /that)
  def /(that:Affine): Affine = if (that.A.isEmpty) 
      this/that.b
    else
      throw new RuntimeException("Nonlinear") 
  
} 

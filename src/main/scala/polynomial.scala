package my.polynomial

import scala.annotation._
import scala.math._
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
}

object RationalConversion {
  // Implicit conversion from integer
  implicit def fromInt(x:Int) = Rational(x, 1)
}  

package my.polynomial

import scala.annotation._
import scala.math._
import scala.collection.immutable.Map
import my.ergo._

class Rational(n:Int, d:Int)
{
  @tailrec private def gcd(x:Int, y:Int): Int = 
    if (x==0) y else gcd(y%x, x)

  private def normalize(n:Int,d:Int) = {
    if (d==0) throw new RuntimeException("Divide by zero")
    val g = gcd(n,d)
    val nn = n/g
    val nd = if (nn == 0) 1 else d/g // 0/k reduce to 0/1
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
  def uminus = new Rational( -num, den)

  override def equals(that: Any) = {
    that match {
      case y:Rational => (num == y.num) && (den == y.den)
      case y:Int => this.equals(new Rational(y,1))
      case _ => false
    }
  }
}

object Rational {
  def apply(n:Int, d:Int) = new Rational(n,d)
}

object RationalConversion {
  // Implicit conversion from integer
  implicit def fromInt(x:Int) = Rational(x, 1)
}

case class V(val name:String) {
}

// Affine forms ax + b 
case class Affine (vars:Map[V,Rational], val b:Rational) {
  private def normalize(s:Map[V,Rational]):Map[V,Rational] = {
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

  def subst(x:V, v:Rational): Affine = {
    val coeff = A(x)
    Affine(A - x,  coeff * v + b)
  }

  def subst(x:V, v:Affine): Affine = {
    if (A.contains(x)) {
     val coeff = A(x)
      new Affine(A - x, b) + (v * coeff)
    } else 
      this  
    
  }

  def solve: (Option[(V, Affine)], Solution) = {
    if (A.isEmpty) 
      if (b.isZero) (None, Yes) else (None, No)
    else {
      val (x, e) = A.head
      (Some(x, Affine(A.drop(1), b).uminus / e ), Yes)
      
    }
  }

  def uminus: Affine = Affine(A.mapValues(e => e.uminus), b.uminus)

  def isZero: Boolean = A.isEmpty && (b == Rational(0,1))
  def isValue: Boolean = A.isEmpty 
}


class AffineRep ( val f:Affine) extends Rep[AffineRep] {
  override def leaves: List[HashedTerm] = {
    f.A.keys.toList.map{ 
      case V(n) => {
        val s = HashConSymbol.make(Var(n), SReal)
        HashConTerm.make(s.sv, List[HashedTerm]())
      }  
    }
  }
  override def equal(that:AffineRep) = (f == that.f)

  override def equals(other:Any): Boolean = {
    other match {
      case that:AffineRep => f == that.f
      case _ => false
    }
  }
  override def subst(x:HashedTerm, v:AffineRep) = {
    val xx = x.t.f.symb match {
      case Var(n) => V(n)
      case _ => throw new RuntimeException("Unknown term") 
    }
    new AffineRep(f.subst(xx, v.f)) 
  }
  

  override def solve(y:AffineRep) = {
    (f - y.f).solve match {
      case (Some((V(v), r)), Yes) => {
        val s = HashConSymbol.make(Var(v), SReal)
        val x = HashConTerm.make(s.sv, List())
        (List((x, new AffineRep(r))), Yes)
      }
      case (_, No) => (List[(HashedTerm, AffineRep)](), No)
      case (None, Yes) => (List[(HashedTerm, AffineRep)](), Yes)
    }
  } 
  
  override def toString = f.toString
  // These are required because the 
  override def hashCode = f.hashCode
}

object AffineRep extends Factory[AffineRep] {
  import RationalConversion._

  private def value(v:Rational) = new AffineRep(Affine(Map[V,Rational](), v))
  private def variable(n:String) = new AffineRep(Affine(Map(V(n)->Rational(1,1)),0))
  
  private def extract(l:List[HashedTerm]) = { 
    (l.head, l.tail.head)
  }

  def make (term:HashedTerm): AffineRep = {
    term.t.f.symb match {
      case Name(n) => throw new RuntimeException("Names not supported")
      case Var(n) =>  variable(n)
      case Plus => {
        val(l,r) = extract(term.t.xs)
        new AffineRep(make(l).f + make(r).f)
      }
      case Minus => {
        val(l,r) = extract(term.t.xs)
        new AffineRep(make(l).f - make(r).f)
      }
      case Multiply => {
        val(l,r) = extract(term.t.xs)
        new AffineRep(make(l).f * make(r).f)
      }
      case Divide => {
        val(l,r) = extract(term.t.xs)
        new AffineRep(make(l).f / make(r).f)
      }
      case Const(i) => value(i)
    }
  }
}

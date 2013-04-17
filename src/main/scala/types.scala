package my.ergo

import my.util._
import scala.math.Ordering

/* Binary operators */
abstract class Binop 
object Plus extends Binop
object Minus extends Binop
object Multiply extends Binop
object Divide extends Binop

/** Symbol can be a name, a var, a Const int or a binary operator */
abstract class Symbol
case class Name (name:String) extends Symbol
case class Var (name:String) extends Symbol
case class BinopSymbol (op:Binop) extends Symbol
case class Const (id:Int) extends Symbol

object Symbol {
  def symbolToString: Symbol=>String = {
    case Name(n) => n
    case Var(x) => x
    case BinopSymbol(Plus) => "+"
    case BinopSymbol(Minus) => "-"
    case BinopSymbol(Multiply) => "*"
    case BinopSymbol(Divide) => "/" 
    case Const(id) => id.toString
  }

}


/** Types of symbol */
sealed abstract class SymbolType 
object SInt extends SymbolType
object SBool extends SymbolType
object SReal extends SymbolType
case class SUnknown(name:String) extends SymbolType /* A symbol whose type is not determined */

/* Convenient object */
object SymbolType {
  def typeToString: SymbolType=>String = {
    case SInt => "int"
    case SBool => "bool"
    case SReal => "real"
    case SUnknown(s) => "`"+ s
  }
}

// Main class is called SymbolView, it has a symbol representing its name and a 
// type.
case class SymbolView(val symb:Symbol, val stype:SymbolType) extends HashedType {
  /** Equality of symbol view is defined as 
     - For variables, they are equal if the types are identical
     - Otherwise, they are equal if their type match and the symbols are identical
  */
  override def equal(other: HashedType) = {
    def eqType(t1:SymbolType, t2:SymbolType) = {
      (t1,t2) match {
        case (_,SUnknown(_)) => true
        case (SUnknown(_), _) => true
        case (SBool, SBool) => true
        case (SReal, SReal) => true
        case (SInt, SInt) => true
        case _ => false
      }
    }
    other match {
      case SymbolView(s, t) => {
        (symb, s) match {
          case (Var(_), Var(_))=> (stype == t)  /* Variables of same type */
          case (_, _) => eqType(stype, t) && (symb == s) /* Same symbol and equal type */
        }
      }
      case _ => false
    }
  }

  override def hash = symb.hashCode 
  def eqType(t:SymbolType) = (stype == t)

  override def toString = Symbol.symbolToString(symb) + ":" + SymbolType.typeToString(stype)
}

// A Symbol with its hash tag
class HashedSymbol(val sv:SymbolView, val tag:Int) {
  def equal(other:HashedSymbol) = (tag == other.tag)
 
  def isConst = {
    this.sv.symb match {
      case Const(_) => true
      case _ => false
    }
  }

  def isVar = {
    this.sv.symb match {
      case Var(_) => true
      case _ => false
    }
  }

  def value = {
    this.sv.symb match {
      case Const(id) => id
      case _ => throw new RuntimeException("Not found")
    }
  }

  def isCommutative = false
  def isAssociative = false
}

object HashConSymbol {
  val tbl = new HashCons[SymbolView](1000)
  def make(s:Symbol, t: SymbolType) = {
    val (sv, tag) = tbl.hashCons( SymbolView(s,t)) 
    new HashedSymbol(sv, tag)
  }
  def equal(s1:HashedSymbol, s2:HashedSymbol) = s1.equal(s2)
  def hash(sv:HashedSymbol) = sv.tag
  def clear = tbl.createTable(1000)
}

// Terms 
// Note the constructor refers to HashedTerms which is defined later
case class Term(val f:SymbolView, val xs:List[HashedTerm]) extends HashedType {
  override def hash = this.hashCode
  override def equal(other: HashedType):Boolean = {
    if (other.isInstanceOf[Term]) {
      val that = other.asInstanceOf[Term]
      f.equal(that.f) && (xs, that.xs).zipped.forall( (x,y) => x.equal(y))
    } else {
      false
    }
  } 
}

class HashedTerm(val t:Term, val tag:Int) {
  def equal(other:HashedTerm) = (tag == other.tag)

  def subst(s:Map[SymbolView,HashedTerm]):HashedTerm = {
    if (s.contains(t.f)) 
      // Maps (variables) to its values
      s(t.f)
    else 
      HashConTerm.make(t.f, t.xs.map(x => 
        x.subst(s) ) )
  }

  def hash = tag
}

object HashedTermOrdering extends Ordering[HashedTerm] {
  def compare(t1: HashedTerm, t2: HashedTerm) = t1.tag - t2.tag
}

object HashConTerm {
  val tbl = new HashCons[Term](1000)
  def make(sym:SymbolView, xs:List[HashedTerm]) = {
    val (v, tag) = tbl.hashCons(Term(sym, xs))
    new HashedTerm(v, tag)
  }
  def equal(h1:HashedTerm, h2:HashedTerm) = h1.equal(h2)
  def hashCode(ht:HashedTerm) = ht.tag
  def clear = tbl.createTable(1000)

  
}
